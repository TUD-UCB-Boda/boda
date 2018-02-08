// XXX include ordering important?
#include"rtc_compute.H"
#include "str_util.H"
#include "timers.H"
#include "vulkan.h"
#include <iostream>
#include <shaderc/shaderc.hpp>
#include <stdlib.h>
#include <list>

//#define DEBUG
//#define DIRECT_GLSL
//#define CLSPV
#define MAX_KERNELS_PER_BUFFER 1

namespace boda {
  // XXX improve/implement error handling
#define BAIL_ON_BAD_RESULT(result) \
  if (VK_SUCCESS != (result)) { fprintf(stderr, "Failure at %u %s\n", __LINE__, __FILE__); exit(-1); }


  string vk_base_decls = R"rstr(
#version 450
#define GLSL
// XXX no typedef in GLSL
#define uint32_t uint
#define int32_t int
// XXX no native support for char/uint8_t
#define uint8_t uint

#define CUCL_BACKEND_IX 3
const uint32_t U32_MAX = 0xffffffff;
const float FLT_MAX = /*0x1.fffffep127f*/ 340282346638528859811704183484516925440.0f;
const float FLT_MIN = 1.175494350822287507969e-38f;
// XXX figure this out later
//typedef long long int64_t;
#define CUCL_GLOBAL_KERNEL
#define CUCL_DEVICE
#define GASQ
#define GLOB_ID_1D (gl_GlobalInvocationID.x)
#define LOC_ID_1D (gl_LocalInvocationID.x)
#define GRP_ID_1D (gl_WorkGroupID.x)
#define LOC_SZ_1D (gl_WorkGroupSize.x)
// shared variables are defined outside of the function
#define LOCSHAR_MEM
#define LSMASQ
// XXX check whether we need both barriers
#define BARRIER_SYNC groupMemoryBarrier(); barrier();

// note: it seems GLSL doesn't provide powf(), but instead overloads pow() for double and float.
// so, we use this as a compatibility wrapper.
#define powf(v,e) pow(v,e)
// XXX figure this out later
//#define store_float_to_rp_half( val, ix, p ) vstore_half( val, ix, p )
#define store_float_to_rp_float( val, ix, p ) p[ix] = val

#define START_ARG
#define END_ARG ;
#define FLOAT_CAST
#define INT_CAST(x) int(x)

)rstr";

  struct vk_func_info_t {
    rtc_func_info_t info;
    VkShaderModule kern;
    VkPipelineLayout pipeline_layout;
    VkPipeline pipeline;
    VkDescriptorSetLayout layout;
    vector<bool> is_buffer;
    #ifdef DEBUG
    uint32_t tpb;
    #endif
  };

  typedef map< string, vk_func_info_t > map_str_vk_func_info_t;
  typedef shared_ptr< map_str_vk_func_info_t > p_map_str_vk_func_info_t;

  // XXX find better name
  struct vk_buffer_info_t {
    VkBuffer buf;
    VkDeviceMemory mem;
  };

  typedef vector<vk_buffer_info_t> vect_vk_buffer_info_t;

  struct vk_var_info_t {
    vk_buffer_info_t bo;
    dims_t dims;
  };

  struct command_buffer_t {
    VkCommandBuffer vk_cb;
    VkFence fence;
    std::list<vk_buffer_info_t> pods;
    std::list<VkDescriptorPool> pools;
  };

  typedef map < string, vk_var_info_t > map_str_vk_var_info_t;
  typedef shared_ptr< map_str_vk_var_info_t > p_map_str_vk_var_info_t;

  typedef vector< VkQueryPool > vect_vk_query_pool_t;

  struct vk_compute_t : virtual public nesi, public rtc_compute_t // NESI(help="Vulkan based rtc support",
			// bases=["rtc_compute_t"], type_id="vk" )
  {

    VkInstance instance;

    VkDebugReportCallbackEXT debug_report_callback;

    VkDevice device;

    VkPhysicalDeviceMemoryProperties mem_properties;

    vk_buffer_info_t staging_buffer;
    vk_buffer_info_t null_buffer;
    size_t staging_sz = 0;

    VkQueue queue;

    uint32_t queue_family_index;

    VkCommandPool command_pool;
    command_buffer_t* current_command_buffer = 0;
    std::list<command_buffer_t> command_buffers;
    int submitted_kernels = 0;
    int kernels_per_buffer = 1;
    zi_bool init_done;

    p_map_str_vk_var_info_t vis;
    p_map_str_vk_func_info_t kerns;

    virtual cinfo_t const * get_cinfo( void ) const; // required declaration for NESI support

    static VKAPI_ATTR VkBool32 VKAPI_CALL debug_callback(
							VkDebugReportFlagsEXT flags,
							VkDebugReportObjectTypeEXT objType,
							uint64_t obj,
							size_t location,
							int32_t code,
							const char* layer_prefix,
							const char* msg,
							void* user_data) {

      std::cout << "validation layer: " << msg << std::endl;

      return VK_FALSE;
    }

    void init( void ) {

      const VkApplicationInfo application_info = {
	VK_STRUCTURE_TYPE_APPLICATION_INFO,
	0,
	"boda",
	0,
	"",
	0,
	VK_MAKE_VERSION(1, 0, 61) // XXX check
      };

      vector<const char*> layers;
      vector<const char*> extensions;

#ifdef DEBUG
      layers.push_back("VK_LAYER_LUNARG_standard_validation");
      extensions.push_back(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
#endif
      // XXX maybe use device extensions and layers
      const VkInstanceCreateInfo instance_create_info = {
	VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
	0,
	0,
	&application_info,
	(uint32_t) layers.size(),
	layers.data(),
	(uint32_t) extensions.size(),
	extensions.data(),
      };

      BAIL_ON_BAD_RESULT(vkCreateInstance(&instance_create_info, 0, &instance));

#ifdef DEBUG
      const VkDebugReportCallbackCreateInfoEXT debug_callback_create_info = {
	VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
	0,
	VK_DEBUG_REPORT_ERROR_BIT_EXT | VK_DEBUG_REPORT_WARNING_BIT_EXT | VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT,
	debug_callback,
	0};

      // We have to explicitly load this function.
      auto vk_create_debug_report_callback_EXT = (PFN_vkCreateDebugReportCallbackEXT)vkGetInstanceProcAddr(instance, "vkCreateDebugReportCallbackEXT");
      BAIL_ON_BAD_RESULT(vk_create_debug_report_callback_EXT == nullptr ? VK_ERROR_INITIALIZATION_FAILED : VK_SUCCESS);

      // Create and register callback.
      BAIL_ON_BAD_RESULT(vk_create_debug_report_callback_EXT(instance, &debug_callback_create_info, NULL, &debug_report_callback));
#endif

      uint32_t physical_device_count = 0;
      BAIL_ON_BAD_RESULT(vkEnumeratePhysicalDevices(instance, &physical_device_count, 0));

      VkPhysicalDevice* const physical_devices = (VkPhysicalDevice*)malloc(sizeof(VkPhysicalDevice) * physical_device_count);

      BAIL_ON_BAD_RESULT(vkEnumeratePhysicalDevices(instance, &physical_device_count, physical_devices));

      BAIL_ON_BAD_RESULT(physical_device_count ? VK_SUCCESS : VK_ERROR_INITIALIZATION_FAILED);

      uint32_t queue_family_count;

      // XXX just use device 0 for now

      vkGetPhysicalDeviceQueueFamilyProperties(physical_devices[0], &queue_family_count, 0);
      std::vector<VkQueueFamilyProperties> queue_families(queue_family_count);

      vkGetPhysicalDeviceQueueFamilyProperties(physical_devices[0], &queue_family_count, queue_families.data());
      queue_family_index = 0;
      bool found_queue_family = false;
      for (; queue_family_index < queue_family_count; queue_family_index++) {
	VkQueueFamilyProperties props = queue_families[queue_family_index];

	if (props.queueCount > 0 && (props.queueFlags & VK_QUEUE_COMPUTE_BIT)) {
	  found_queue_family = true;
	  break;
	}
      }

      BAIL_ON_BAD_RESULT(found_queue_family ? VK_SUCCESS : VK_ERROR_INITIALIZATION_FAILED);

      // XXX investigate
      const float queue_priority = 1.0f;

      const VkDeviceQueueCreateInfo deviceQueueCreateInfo = {
	VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
	0,
	0,
	queue_family_index,
	1,
	&queue_priority
      };

      vector<const char*> dev_extensions;
      #ifdef DIRECT_GLSL
      dev_extensions.push_back("VK_NV_glsl_shader");
      #endif
      // XXX device extensions and layers
      const VkDeviceCreateInfo device_create_info = {
	VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
	0,
	0,
	1,
	&deviceQueueCreateInfo,
	0,
	0,
	(uint32_t) dev_extensions.size(),
	dev_extensions.data(),
	0
      };

      BAIL_ON_BAD_RESULT(vkCreateDevice(physical_devices[0], &device_create_info, 0, &device));

      vkGetPhysicalDeviceMemoryProperties(physical_devices[0], &mem_properties);

      vkGetDeviceQueue(device, queue_family_index, 0, &queue);


      VkCommandPoolCreateInfo command_pool_create_info = {
	VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
	0,
	VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
	queue_family_index
      };

      BAIL_ON_BAD_RESULT(vkCreateCommandPool(device, &command_pool_create_info, 0, &command_pool));
       
      null_buffer = create_buffer_info(1, // XXX can't create buffers with size 0
				       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
				       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);


      init_done.v = true;
    }

    virtual string get_plat_tag( void ) {
      // XXX improve
      return "vk";
    }


    VkShaderModule getSMCIFromGLSLC(string src, string func_name) {
      shaderc::Compiler compiler;
      shaderc::CompileOptions options;
      //std::cout << "compiling : " << i->func_name << std::endl;

      options.SetOptimizationLevel(shaderc_optimization_level_size);
      shaderc::SpvCompilationResult module =
	compiler.CompileGlslToSpv(src, shaderc_glsl_compute_shader, func_name.c_str(), options);

      if (module.GetCompilationStatus() != shaderc_compilation_status_success) {
	std::cout << "failed to compile: " << func_name << std::endl <<
	  "error message: " << std::endl << module.GetErrorMessage() << std::endl;
	BAIL_ON_BAD_RESULT(VK_INCOMPLETE);
      }

      std::vector<uint32_t> buffer;
      buffer = {module.cbegin(), module.cend()};
	

      VkShaderModuleCreateInfo shader_module_create_info = {
	VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
	0,
	0,
	buffer.size() * sizeof(uint32_t),
	buffer.data()
      };
      VkShaderModule shader_module;
      BAIL_ON_BAD_RESULT(vkCreateShaderModule(device, &shader_module_create_info, 0, &shader_module));
      return shader_module;
    }

    VkShaderModule getSMCIFromPlainGLSL(string src) {
      VkShaderModuleCreateInfo shader_module_create_info = {
	VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
	0,
	0,
	src.length()+1,
	(uint32_t*) src.c_str()
      };
	
      VkShaderModule shader_module;
      BAIL_ON_BAD_RESULT(vkCreateShaderModule(device, &shader_module_create_info, 0, &shader_module));
      return shader_module;
    }

    VkShaderModule getSMCIFromClspv(string src, string func_name, int pos) { 
      ensure_is_dir( gen_src_output_dir.exp, 1 );
      
      p_ostream out = ofs_open( strprintf( "%s/%s_%d.cl", gen_src_output_dir.exp.c_str(), func_name.c_str(), pos));
      (*out) << src << std::flush;
      system(("/home/jschulte/vulkan_backend/clspv_/build/bin/clspv -cluster-pod-kernel-args " +
	      gen_src_output_dir.exp + "/" + func_name + "_" + std::to_string(pos) + ".cl -o " +
	      gen_src_output_dir.exp + "/" + func_name + "_" + std::to_string(pos) + ".spv").c_str());

      system(("/home/jschulte/vulkan/VulkanSDK/1.0.61.1/x86_64/bin/spirv-opt --inline-entry-points-exhaustive --merge-blocks --eliminate-dead-branches --eliminate-dead-code-aggressive " +
	      gen_src_output_dir.exp + "/" + func_name + "_" + std::to_string(pos) + ".spv -o " +
	      gen_src_output_dir.exp + "/" + func_name + "_" + std::to_string(pos) + "_opt.spv").c_str());
      
      std::ifstream is;
      is.open(gen_src_output_dir.exp + "/" + func_name + "_" + std::to_string(pos) + "_opt.spv", std::ios::binary);
      is.seekg(0, std::ios::end);
      size_t size = is.tellg();
      is.seekg(0, std::ios::beg);

      std::vector<uint32_t> buffer;
      buffer.resize(size/sizeof(uint32_t) + (size%sizeof(uint32_t)?1U:0U));
      is.read((char *) buffer.data(), size);
      VkShaderModuleCreateInfo shader_module_create_info = {
	VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
	0,
	0,
	buffer.size() * sizeof(uint32_t),
	buffer.data()
      };
      
      VkShaderModule shader_module;
      BAIL_ON_BAD_RESULT(vkCreateShaderModule(device, &shader_module_create_info, 0, &shader_module));
      return shader_module;
    }
    
    void compile( vect_rtc_func_info_t const & func_infos, rtc_compile_opts_t const & opts) {
      assert (init_done.v);
      timer_t t("vk compile");
      if (func_infos.empty()) return;

      for( vect_rtc_func_info_t::const_iterator i = func_infos.begin(); i != func_infos.end(); ++i ) {
	#ifdef CLSPV
	bool fallback = !i->func_name.compare(0, 4, "pool") || !i->func_name.compare(0,3,"gen");
	string src;
	if (fallback)
	  src = vk_base_decls + get_rtc_base_decls() + i->func_src;
	else
	  src = ocl_base_decls + get_rtc_base_decls() + i->func_src;
	#else
	string src = vk_base_decls + get_rtc_base_decls() + i->func_src;
	if( gen_src ) {
	  ensure_is_dir( gen_src_output_dir.exp, 1 );
	  p_ostream out = ofs_open( strprintf( "%s/%s_%d.glsl", gen_src_output_dir.exp.c_str(), i->func_name.c_str(), (int) (i - func_infos.begin())));
	  (*out) << src << std::flush;
	}
	#endif
	#ifdef CLSPV
	VkShaderModule shader_module;
	if (fallback)
	  shader_module = getSMCIFromGLSLC(src, i->func_name);
	else
	  shader_module = getSMCIFromClspv(src, i->func_name, i - func_infos.begin());
	#else
	#ifdef DIRECT_GLSL
	VkShaderModule shader_module = getSMCIFromPlainGLSL(src);
	#else
	VkShaderModule shader_module = getSMCIFromGLSLC(src, i->func_name);
        #endif
	#endif

	vector<VkDescriptorSetLayoutBinding> var_bindings;
	size_t bind_ix = 0;
	vector<bool> is_buffer;
	// XXX the whole arguments-handling code is rather messy and should be replaced with a cleaner/nicer solution
	for (vect_arg_decl_t::const_iterator arg = i->arg_decls.begin(); arg != i->arg_decls.end(); ++arg ) {
	  uint32_t const multi_sz = arg->get_multi_sz( i->op );
	  for( uint32_t mix = 0; mix != multi_sz; ++mix ) {
	    if (arg->loi.v == 0 || arg->io_type == "REF"){ // XXX handle DYN-REF
	      //std::cout << "skipped " << arg->vn << std::endl;
	      is_buffer.push_back(false);
	      break;
	    }

	    //std::cout << "added " << arg->vn << std::endl;
	    is_buffer.push_back(true);
	    // This argument will be passed via a storage buffer, so we setup its descriptor layout
	    var_bindings.push_back({(uint32_t)bind_ix,
		  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
		  1,
		  VK_SHADER_STAGE_COMPUTE_BIT,
		  0});
	    bind_ix++;
	  }
	}

	// one binding to pass all non-var args
	// XXX not necessary if such args are not present
	var_bindings.push_back({(uint32_t)bind_ix,
	      VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
	      1,
	      VK_SHADER_STAGE_COMPUTE_BIT,
	      0});
	bind_ix++;
	
	VkDescriptorSetLayoutCreateInfo layout_create_info = {
	  VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
	  0,
	  0,
	  (uint32_t)bind_ix,
	  var_bindings.data()
	};

	VkDescriptorSetLayout layout;
	BAIL_ON_BAD_RESULT(vkCreateDescriptorSetLayout(device, &layout_create_info, 0, &layout));
      
	std::vector<VkDescriptorSetLayout> layouts;
	layouts.push_back(layout);

	VkPipelineLayoutCreateInfo pipeline_layout_create_info = {
	  VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
	  0,
	  0,
	  1,
	  layouts.data(),
	  0,
	  0
	};

	VkPipelineLayout pipeline_layout;
        BAIL_ON_BAD_RESULT(vkCreatePipelineLayout(device, &pipeline_layout_create_info, 0, &pipeline_layout));
	
	size_t const loc_work_sz = i->tpb;
	const VkSpecializationMapEntry entries[] =
	// id,  offset,                size
	  {{0, 0, sizeof(size_t)}};

	const VkSpecializationInfo spec_info = {
	  1,                  // mapEntryCount
	  entries,            // pMapEntries
	  1 * sizeof(size_t),  // dataSize
	  &loc_work_sz               // pData
	};

	VkComputePipelineCreateInfo compute_pipeline_create_info = {
	  VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
	  0,
	  0,
	  {
	    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
	    0,
	    0,
	    VK_SHADER_STAGE_COMPUTE_BIT,
	    shader_module,
	    #ifdef CLSPV
	    fallback ? "main" : i->func_name.c_str(),
	    #else
	    "main",
	    #endif
	    &spec_info
	  },
	  pipeline_layout,
	  0,
	  0
	};

	VkPipeline pipeline;
	//BAIL_ON_BAD_RESULT(vkCreateComputePipelines(device, 0, 1, &compute_pipeline_create_info, 0, &pipeline));
	
	if (vkCreateComputePipelines(device, 0, 1, &compute_pipeline_create_info, 0, &pipeline) != VK_SUCCESS)
	  printf("error");

	must_insert(*kerns, i->func_name, vk_func_info_t{*i, shader_module, pipeline_layout, pipeline, layout, is_buffer
	      #ifdef DEBUG
	      , (uint32_t) loc_work_sz
	      #endif
	      });
      }
    }

    // buf and mem of return value must be destroyed/freed
    vk_buffer_info_t create_buffer_info(VkDeviceSize size, VkBufferUsageFlags buffer_flags, VkMemoryPropertyFlags memory_flags) {
      const VkBufferCreateInfo buffer_create_info = {
	VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
	0,
	0,
	size,
	buffer_flags,
	VK_SHARING_MODE_EXCLUSIVE,
	1,
	&queue_family_index
      };

      VkBuffer buf;
      BAIL_ON_BAD_RESULT(vkCreateBuffer(device, &buffer_create_info, 0, &buf));

      // set memory_type_index to an invalid entry in the properties.memoryTypes array
      uint32_t memory_type_index = VK_MAX_MEMORY_TYPES;

      VkMemoryRequirements mem_reqs;
      vkGetBufferMemoryRequirements(device, buf, &mem_reqs);

      for (uint32_t k = 0; k < mem_properties.memoryTypeCount; k++) {
	if ((memory_flags & mem_properties.memoryTypes[k].propertyFlags) == memory_flags &&
	    (mem_reqs.memoryTypeBits & (1 << k))) {
	  memory_type_index = k;
	  break;
	}
      }

      BAIL_ON_BAD_RESULT(memory_type_index == VK_MAX_MEMORY_TYPES ? VK_ERROR_OUT_OF_HOST_MEMORY : VK_SUCCESS);

      const VkMemoryAllocateInfo memory_allocate_info = {
	VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
	0,
	mem_reqs.size,
	memory_type_index
      };
      VkDeviceMemory dev_mem;
      BAIL_ON_BAD_RESULT(vkAllocateMemory(device, &memory_allocate_info, 0, &dev_mem));

      BAIL_ON_BAD_RESULT(vkBindBufferMemory(device, buf, dev_mem, 0));

      return {buf, dev_mem};
    }

    void setup_staging_buffer(size_t size) {
      if (size <= staging_sz)
	return;
      if (staging_sz != 0) {
	vkFreeMemory(device, staging_buffer.mem, 0);
	vkDestroyBuffer(device, staging_buffer.buf, 0);
      }
      staging_buffer = create_buffer_info(size,
					  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_SRC_BIT |
					  VK_BUFFER_USAGE_TRANSFER_DST_BIT,
					  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
      staging_sz = size;
    }


    void buffer_copy(VkBuffer dst, VkBuffer src, VkDeviceSize size) {
      finish_and_sync();
      // XXX maybe use current_command_buffer here as well
      VkCommandBufferAllocateInfo command_buffer_allocate_info = {
	VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
	0,
	command_pool,
	VK_COMMAND_BUFFER_LEVEL_PRIMARY,
	1
      };
      VkCommandBuffer command_buffer;
      BAIL_ON_BAD_RESULT(vkAllocateCommandBuffers(device, &command_buffer_allocate_info, &command_buffer));
      VkCommandBufferBeginInfo command_buffer_begin_info = {
	VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
	0,
	VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
	0
      };

      BAIL_ON_BAD_RESULT(vkBeginCommandBuffer(command_buffer, &command_buffer_begin_info));

      VkBufferCopy copy = {0, 0, size};
      vkCmdCopyBuffer(command_buffer, src, dst, 1, &copy);

      BAIL_ON_BAD_RESULT(vkEndCommandBuffer(command_buffer));


      VkSubmitInfo submit_info = {
	VK_STRUCTURE_TYPE_SUBMIT_INFO,
	0,
	0,
	0,
	0,
	1,
	&command_buffer,
	0,
	0
      };
      BAIL_ON_BAD_RESULT(vkQueueSubmit(queue, 1, &submit_info, 0));
      BAIL_ON_BAD_RESULT(vkQueueWaitIdle(queue));
    }

    void copy_nda_to_var (string const &vn, p_nda_t const & nda) {

      vk_var_info_t const & vi = must_find( *vis, vn );
      assert( nda->dims == vi.dims);

      setup_staging_buffer(nda->dims.bytes_sz());
      void *dev_ptr;
      BAIL_ON_BAD_RESULT(vkMapMemory(device, staging_buffer.mem, 0, nda->dims.bytes_sz(), 0, (void **)&dev_ptr));
      memcpy(dev_ptr, nda->rp_elems(), nda->dims.bytes_sz());
      vkUnmapMemory(device, staging_buffer.mem);

      buffer_copy(vi.bo.buf, staging_buffer.buf, nda->dims.bytes_sz());
    }

    void copy_var_to_nda (p_nda_t const & nda, string const &vn) {

      vk_var_info_t const & vi = must_find( *vis, vn );
      assert( nda->dims == vi.dims);
      setup_staging_buffer(nda->dims.bytes_sz());
      buffer_copy(staging_buffer.buf, vi.bo.buf, nda->dims.bytes_sz());

      void *dev_ptr;
      BAIL_ON_BAD_RESULT(vkMapMemory(device, staging_buffer.mem, 0, nda->dims.bytes_sz(), 0, (void **)&dev_ptr));
      memcpy( nda->rp_elems(), dev_ptr, nda->dims.bytes_sz());
      vkUnmapMemory(device, staging_buffer.mem);
    }

    p_nda_t get_var_raw_native_pointer( string const & vn ) {
      // XXX necessary?
      rt_err( "vk_compute_t: get_var_raw_native_pointer(): not implemented");
    }

    void create_var_with_dims( string const & vn, dims_t const & dims ) {
      vk_var_info_t var;
      assert(init_done.v);
      vk_buffer_info_t bo = create_buffer_info(dims.bytes_sz(),
						 VK_BUFFER_USAGE_STORAGE_BUFFER_BIT |
						 VK_BUFFER_USAGE_TRANSFER_DST_BIT |
						 VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
						 VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

      must_insert( *vis, vn, vk_var_info_t{bo, dims});

      set_var_to_zero( vn );

    }

    void create_var_with_dims_as_reshaped_view_of_var( string const & vn, dims_t const & dims, string const & src_vn ) {
      vk_var_info_t const & src_vi = must_find( *vis, src_vn );
      rtc_reshape_check( dims, src_vi.dims );
      must_insert( *vis, vn, vk_var_info_t{ src_vi.bo, dims } );
    }

    void release_var( string const & vn ) {
      vk_var_info_t var = must_find( *vis, vn );
      vkFreeMemory(device, var.bo.mem, 0);
      vkDestroyBuffer(device, var.bo.buf, 0);
      must_erase( *vis, vn );
    }
    virtual void release_all_funcs( void ) {
      for (auto& f : *kerns) {
	vk_func_info_t func = must_find( *kerns, f.first );
	vkDestroyShaderModule(device, func.kern, 0);
	vkDestroyDescriptorSetLayout(device, func.layout, 0);
	vkDestroyPipeline(device, func.pipeline, 0);
	vkDestroyPipelineLayout(device, func.pipeline_layout, 0);
      }
      kerns->clear();
    }

    dims_t get_var_dims( string const & vn ) { return must_find( *vis, vn ).dims; }

    void set_var_to_zero(string const & vn ) {
      vk_var_info_t var = must_find( *vis, vn );
      void* mem = malloc(var.dims.bytes_sz());
      memset(mem, 0, var.dims.bytes_sz());
      p_nda_t nda = make_shared<nda_t>(var.dims, mem);
      copy_nda_to_var(vn, nda);
      free(mem);
    }

    vk_compute_t( void ) : vis( new map_str_vk_var_info_t ), kerns( new map_str_vk_func_info_t ) { }

    vect_vk_query_pool_t call_evs;
    uint32_t alloc_call_id( void ) {
      VkQueryPoolCreateInfo query_info = {
	VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
	0,
	0,
	VK_QUERY_TYPE_TIMESTAMP,
	2,
	0
      };
      VkQueryPool query_pool;
      call_evs.push_back(query_pool);

      vkCreateQueryPool(device, &query_info, 0, &call_evs[call_evs.size()-1]);

      return call_evs.size() -1;
    }
    virtual void release_per_call_id_data( void ) {
      for (VkQueryPool pool : call_evs) {
	vkDestroyQueryPool(device, pool, 0);
      }
      call_evs.clear();
    } // invalidates all call_ids inside rtc_func_call_t's

    virtual float get_dur( uint32_t const & b, uint32_t const & e ) {
      uint64_t timestamps_b[2];
      BAIL_ON_BAD_RESULT(vkGetQueryPoolResults(device, call_evs[b], 0, 2, 2*sizeof(uint64_t),
					       &(timestamps_b[0]), sizeof(uint64_t),  VK_QUERY_RESULT_64_BIT | VK_QUERY_RESULT_WAIT_BIT));
      
      uint64_t timestamps_e[2];
      BAIL_ON_BAD_RESULT(vkGetQueryPoolResults(device, call_evs[e], 0, 2, 2*sizeof(uint64_t),
					       &(timestamps_e[0]), sizeof(uint64_t),  VK_QUERY_RESULT_64_BIT | VK_QUERY_RESULT_WAIT_BIT));
      uint64_t dur_i = timestamps_e[1]-timestamps_b[0];
      float dur_f = ((float)dur_i)*1.0e-6;

      return dur_f;
    }

    virtual float get_var_compute_dur( string const & vn ) { return 0; }
    virtual float get_var_ready_delta( string const & vn1, string const & vn2 ) { return 0; }

    void release_func( string const & func_name ) {
      vk_func_info_t func = must_find( *kerns, func_name );
      vkDestroyShaderModule(device, func.kern, 0);
      vkDestroyDescriptorSetLayout(device, func.layout, 0);
      vkDestroyPipeline(device, func.pipeline, 0);
      vkDestroyPipelineLayout(device, func.pipeline_layout, 0);
      must_erase( *kerns, func_name );
    }

    void store_cb_data(vk_buffer_info_t* pod, VkDescriptorPool pool) {
	current_command_buffer->pools.push_back(pool);
	if (pod)
	  current_command_buffer->pods.push_back(*pod);
	return;
    }

    void free_cb_data(command_buffer_t& cb) { 
      for (auto& pool : cb.pools)
	vkDestroyDescriptorPool(device, pool, 0);
      cb.pools.clear();
	
      for (auto& pod : cb.pods) {
	vkFreeMemory(device, pod.mem, 0);
	vkDestroyBuffer(device, pod.buf, 0);
      }
      cb.pods.clear();
      return;
    }
    void set_current_command_buffer() {
      if (current_command_buffer) 
	return;
      
      for (auto& cb: command_buffers) {
	if (vkGetFenceStatus(device, cb.fence) != VK_SUCCESS)
	  continue;
	vkResetFences(device, 1, &cb.fence);
	free_cb_data(cb);
	VkCommandBufferBeginInfo command_buffer_begin_info = {
	  VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
	  0,
	  VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
	  0
	};

	BAIL_ON_BAD_RESULT(vkBeginCommandBuffer(cb.vk_cb, &command_buffer_begin_info));
	
	submitted_kernels = 0;
	current_command_buffer = &cb;
	return;
      }

      command_buffer_t new_cb;
      
      VkCommandBufferAllocateInfo command_buffer_allocate_info = {
	VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
	0,
	command_pool,
	VK_COMMAND_BUFFER_LEVEL_PRIMARY,
	1
      };
      VkCommandBuffer new_vk_cb;
      BAIL_ON_BAD_RESULT(vkAllocateCommandBuffers(device, &command_buffer_allocate_info, &new_vk_cb));
      
      VkCommandBufferBeginInfo command_buffer_begin_info = {
	VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
	0,
	VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
	0
      };

      BAIL_ON_BAD_RESULT(vkBeginCommandBuffer(new_vk_cb, &command_buffer_begin_info));
      new_cb.vk_cb = new_vk_cb;

      VkFenceCreateInfo fence_create_info = {
	VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
	0,
	0
      };
      VkFence new_fence;
      vkCreateFence(device, &fence_create_info, 0, &new_fence);
      new_cb.fence = new_fence;

      command_buffers.push_back(new_cb);
      current_command_buffer = &command_buffers.back();
      submitted_kernels = 0;
    }

    void submit_command(bool force) {
      if (!force && submitted_kernels < kernels_per_buffer)
	return;
      if (!current_command_buffer)
	return;
      
      BAIL_ON_BAD_RESULT(vkEndCommandBuffer(current_command_buffer->vk_cb));

      VkSubmitInfo submit_info = {
	VK_STRUCTURE_TYPE_SUBMIT_INFO,
	0,
	0,
	0,
	0,
	1,
	&current_command_buffer->vk_cb,
	0,
	0
      };
      timer_t t("vk kernel");
      BAIL_ON_BAD_RESULT(vkQueueSubmit(queue, 1, &submit_info, current_command_buffer->fence));
      BAIL_ON_BAD_RESULT(vkQueueWaitIdle(queue));
      t.stop();
      current_command_buffer = nullptr;
      if (kernels_per_buffer < MAX_KERNELS_PER_BUFFER)
	kernels_per_buffer *= 2;
    }
    
    uint32_t run( rtc_func_call_t const & rfc ) {
      // XXX remove debug timers
      timer_t t1("vk run");
      vk_func_info_t const & vfi = must_find(*kerns, rfc.rtc_func_name.c_str());
      uint32_t var_ix = 0;
      
      vector<rtc_arg_t> POD_args;
      vector<rtc_arg_t> var_args;
      size_t POD_sz = 0;
      //std::cout << "running " << rfc.rtc_func_name << std::endl;
      vector<bool>::const_iterator is_buffer = vfi.is_buffer.begin();

      for( vect_string::const_iterator i = vfi.info.arg_names.begin(); i != vfi.info.arg_names.end(); ++i ) {
	map_str_rtc_arg_t::const_iterator ai = rfc.arg_map.find( *i );
	if( ai == rfc.arg_map.end() )
	  { rt_err( strprintf( "vk_compute_t: arg '%s' not found in arg_map for call.\n",
			       str((*i)).c_str() ) ); }

	rtc_arg_t arg = ai->second;
	if (is_buffer != vfi.is_buffer.end() && *is_buffer) {
	  //std::cout << arg.n << " is var" << std::endl;
	  var_args.push_back(arg);
	  var_ix++;
	} else if (arg.is_nda()) {
  	  //std::cout << arg.n << " is nda" << std::endl;
	  POD_args.push_back(arg);
	  // GLSL has no datatypes smaller than 4 bytes
	  POD_sz += (arg.v->rp_elems() ? arg.v->dims.bytes_sz() : 4);
	} else {
	  /* assumes that no pass-by-ptr arguments are created with CUCL, and that CUCL-generated 
	     parameters are always at the end of the arguments list
	   */
	  assert(false); 
	}
	if (is_buffer != vfi.is_buffer.end())
	  ++is_buffer;
      }
      int POD_count = POD_sz ? 1 : 0;

      VkDescriptorPoolSize descriptor_pool_size = {
	VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
	/*
	 * We don't know at compile time, whether we need a descriptor for a POD buffer,
	 * so layout always includes one. May be fixable.
	 */
	(uint32_t) var_ix+1
      };

      VkDescriptorPoolCreateInfo descriptor_pool_create_info = {
	VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
	0,
	0,
	1,
	1,
	&descriptor_pool_size
      };

      VkDescriptorPool descriptor_pool;
      BAIL_ON_BAD_RESULT(vkCreateDescriptorPool(device, &descriptor_pool_create_info, 0, &descriptor_pool));


      VkDescriptorSetAllocateInfo descriptor_set_allocate_info = {
	VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
	0,
	descriptor_pool,
	1,
	&vfi.layout
      };

      VkDescriptorSet descriptor_set;
      BAIL_ON_BAD_RESULT(vkAllocateDescriptorSets(device, &descriptor_set_allocate_info, &descriptor_set));
      vector<VkWriteDescriptorSet> desc_sets_update(var_ix+POD_count);
      vector<VkDescriptorBufferInfo> bi(var_ix+POD_count); // XXX no need for vector
      var_ix = 0;
      //std::cout << rfc.rtc_func_name << " " << UBO_sz << " " << UBO_count <<  std::endl;
      
      for (rtc_arg_t arg : var_args) {
	VkBuffer buf = arg.is_var() ? must_find(*vis, arg.n).bo.buf : null_buffer.buf;
	bi[var_ix] = {
	  buf,
	  0,
	  VK_WHOLE_SIZE};

	desc_sets_update[var_ix] = {
	  VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
	  0,
	  descriptor_set,
	  var_ix,
	  0,
	  1,
	  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
	  0,
	  &bi[var_ix],
	  0};
	var_ix++;
      }
      assert(var_ix == bi.size()-POD_count);

      vk_buffer_info_t pod;
      if (POD_count) {
	pod = create_buffer_info(POD_sz, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
				 VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

	char* dev_ptr;
	BAIL_ON_BAD_RESULT(vkMapMemory(device, pod.mem, 0, POD_sz, 0, (void **) &dev_ptr));
	size_t offset = 0;
	for (rtc_arg_t arg : POD_args) {
	  if (arg.v->rp_elems()) {
	    memcpy(dev_ptr+offset, arg.v->rp_elems(), arg.v->dims.bytes_sz());
	    offset += arg.v->dims.bytes_sz();
	  } else {
	    offset += 4;
	  }
	}

	vkUnmapMemory(device, pod.mem);

	bi[var_ix] = {
	  pod.buf,
	  0,
	  VK_WHOLE_SIZE
	};

	desc_sets_update[var_ix] = {
	  VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
	  0,
	  descriptor_set,
	  var_ix,
	  0,
	  1,
	  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
	  0,
	  &bi[var_ix],
	  0};
      }
      vkUpdateDescriptorSets(device, var_ix+POD_count, desc_sets_update.data(), 0, 0);

      uint32_t const call_id = alloc_call_id();

      size_t const glob_work_sz = rfc.tpb.v*rfc.blks.v;
      size_t const loc_work_sz = rfc.tpb.v;
      #ifdef DEBUG
      assert(loc_work_sz == vfi.tpb); // XXX: does this hold?
      #endif

      set_current_command_buffer();
      store_cb_data(POD_count ? &pod : nullptr, descriptor_pool);
      
      vkCmdResetQueryPool(current_command_buffer->vk_cb, call_evs[call_id], 0, 1);
      
      vkCmdWriteTimestamp(current_command_buffer->vk_cb, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
			  call_evs[call_id], 0);


      vkCmdBindPipeline(current_command_buffer->vk_cb, VK_PIPELINE_BIND_POINT_COMPUTE, vfi.pipeline);


      vkCmdBindDescriptorSets(current_command_buffer->vk_cb, VK_PIPELINE_BIND_POINT_COMPUTE,
			      vfi.pipeline_layout, 0, 1, &descriptor_set, 0, 0);
      vkCmdDispatch(current_command_buffer->vk_cb, glob_work_sz/loc_work_sz, 1, 1);
      submitted_kernels++;
      
      vkCmdWriteTimestamp(current_command_buffer->vk_cb, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
			  call_evs[call_id], 1);
      timer_t t(rfc.rtc_func_name);
      submit_command(false);
      t.stop();
      return call_id;
    }

    void finish_and_sync( void ) {
      submit_command(true);
      BAIL_ON_BAD_RESULT(vkQueueWaitIdle(queue));
    }

    ~vk_compute_t() {
      if (staging_sz != 0) {
	vkFreeMemory(device, staging_buffer.mem, 0);
	vkDestroyBuffer(device, staging_buffer.buf, 0);
      }

      if (!init_done.v)
	return;
      
      vkFreeMemory(device, null_buffer.mem, 0);
      vkDestroyBuffer(device, null_buffer.buf, 0);

      for (auto& v : *vis) {
	vk_var_info_t var = v.second;
	vkFreeMemory(device, var.bo.mem, 0);
	vkDestroyBuffer(device, var.bo.buf, 0);
      }
	vis->clear();

	release_all_funcs();
	for (auto& cb : command_buffers) {
	  free_cb_data(cb);
	  vkDestroyFence(device, cb.fence, 0);
	}
	vkDestroyCommandPool(device, command_pool, 0);

#ifdef DEBUG
	auto func = (PFN_vkDestroyDebugReportCallbackEXT)vkGetInstanceProcAddr(instance, "vkDestroyDebugReportCallbackEXT");
	BAIL_ON_BAD_RESULT(func == nullptr ? VK_ERROR_INITIALIZATION_FAILED : VK_SUCCESS);
	func(instance, debug_report_callback, NULL);
#endif
	vkDestroyDevice(device, 0);

	vkDestroyInstance(instance, 0); 
    }

    // FIXME: TODO
    void profile_start( void ) { }
    void profile_stop( void ) { }
  };

#include"gen/vk_util.cc.nesi_gen.cc"
}
