module Examples.Triangle


open System
open System.IO
open Microsoft.FSharp.NativeInterop
open WGPU.Native
open Examples.Util
open Window

let run () =
    printfn "Creating WGPU instance"
    let instance = Instance()
    printfn "Creating GLFW window"
    use window = new Window(600, 600, "wgpu-native-fs")
    printfn "Creating window surface"
    let surface = window.GetSurface instance
    printfn "Creating graphics adapter"

    let adapter =
        let mutable adapterOptions =
            RequestAdapterOptions(
                compatibleSurface = surface,
                powerPreference = PowerPreference.Undefined
            )

        let mutable adapter = Adapter()

        let callback
            (_status : RequestAdapterStatus)
            (_adapter : Adapter)
            (_message : string)
            (_userdata : nativeint)
            =
            printfn $"{_message}"
            adapter <- _adapter

        instance.RequestAdapter(&&adapterOptions, RequestAdapterCallback callback, IntPtr.Zero)

        adapter

    printfn "Retrieving adapter properties"

    let adapterProps =
        let mutable props = AdapterProperties()
        adapter.GetProperties &props
        props

    printfn
        $"Adapter Properties{Environment.NewLine}{adapterProps.Name} {adapterProps.AdapterType}, {adapterProps.BackendType}, {adapterProps.DriverDescription}, 0x{adapterProps.DeviceID:X16}, 0x{adapterProps.VendorID:X16}"

    let deviceExtras =
        DeviceExtras(
            ChainedStruct(sType = enum<SType> (int NativeSType.DeviceExtras)),
            enum<NativeFeature> 0,
            "Device",
            ""
        )

    use deviceExtrasPtr = intptr.alloc deviceExtras
    let mutable requiredLimits = RequiredLimits(limits = Limits())

    let mutable deviceDescriptor =
        DeviceDescriptor(
            deviceExtrasPtr.ptr,
            "Device",
            0u,
            IntPtr.Zero,
            NativePtr.toNativeInt &&requiredLimits
        )

    let mutable device = Device()
    let deviceCallback _status _device _message _userdata = device <- _device
    printfn "Creating device"

    adapter.RequestDevice(&deviceDescriptor, RequestDeviceCallback deviceCallback, IntPtr.Zero)

    printfn "Creating shader"
    let shaderSource = File.ReadAllText("triangle.wgsl")

    let wgslDescriptor =
        ShaderModuleWGSLDescriptor(
            ChainedStruct(sType = SType.ShaderModuleWGSLDescriptor),
            shaderSource
        )

    let wgslDescriptorPtr = intptr.alloc wgslDescriptor

    let mutable shaderDescriptor =
        ShaderModuleDescriptor(wgslDescriptorPtr.ptr, "shader.wgsl")

    let shader = device.CreateShaderModule &shaderDescriptor

    if shader.handle = IntPtr.Zero then
        printfn "Failed to create shader module"
        exit 1

    printfn "Creating pipeline layout"

    let mutable pipelineLayoutDescriptor =
        PipelineLayoutDescriptor(bindGroupLayoutCount = 0u)

    let pipelineLayout = device.CreatePipelineLayout(&pipelineLayoutDescriptor)
    let swapchainFormat = surface.GetPreferredFormat(adapter)

    let mutable blendState =
        BlendState(
            color = BlendComponent(BlendOperation.Add, BlendFactor.One, BlendFactor.Zero),
            alpha = BlendComponent(BlendOperation.Add, BlendFactor.One, BlendFactor.Zero)
        )

    let blendStatePtr = intptr.alloc blendState

    let mutable colorTargetState =
        ColorTargetState(
            format = swapchainFormat,
            blend = blendStatePtr.ptr,
            writeMask = ColorWriteMask.All
        )

    let colorTargetStatePtr = intptr.alloc colorTargetState

    let mutable fragmentState =
        FragmentState(
            ``module`` = shader,
            entryPoint = "fs_main",
            targetCount = 1u,
            targets = colorTargetStatePtr.ptr
        )

    let fragmentStatePtr = intptr.alloc fragmentState

    let mutable renderPipelineDescriptor =
        RenderPipelineDescriptor(
            label = "Render pipeline",
            layout = pipelineLayout,
            vertex = VertexState(``module`` = shader, entryPoint = "vs_main", bufferCount = 0u),
            primitive =
                PrimitiveState(
                    topology = PrimitiveTopology.TriangleList,
                    stripIndexFormat = IndexFormat.Undefined,
                    frontFace = FrontFace.CCW,
                    cullMode = CullMode.None
                ),
            multisample =
                MultisampleState(count = 1u, mask = UInt32.MaxValue, alphaToCoverageEnabled = false),
            fragment = fragmentStatePtr.ptr
        )

    printfn "Creating pipeline"
    let renderPipeline = device.CreateRenderPipeline &renderPipelineDescriptor

    let recreateSwapchain width height =
        printfn "Creating swapchain"

        let mutable swapchainDescriptor =
            SwapChainDescriptor(
                usage = TextureUsage.RenderAttachment,
                format = swapchainFormat,
                width = width,
                height = height,
                presentMode = PresentMode.Fifo
            )

        device.CreateSwapChain(surface, &swapchainDescriptor)


    let rec loop windowClosing prevWidth prevHeight swapchain =
        if windowClosing then
            printfn "Window closing"
        else
            let width, height = window.GetSize()

            let swapchain =
                if width = prevWidth && height = prevHeight then
                    swapchain
                else
                    recreateSwapchain width height

            let nextTexture = swapchain.GetCurrentTextureView()

            if nextTexture.handle = IntPtr.Zero then
                printfn "Failed to acquire SwapChain texture"

            let mutable encoderDescriptor = CommandEncoderDescriptor(label = "Command Encoder")
            let encoder = device.CreateCommandEncoder &encoderDescriptor
            let mutable colorAttachment =
                RenderPassColorAttachment(
                    view = nextTexture,
                    resolveTarget = Unchecked.defaultof<TextureView>,
                    loadOp = LoadOp.Clear,
                    storeOp = StoreOp.Store,
                    clearValue = Color(r = 0.01, g = 0.01, b = 0.01, a = 1)
                )

            let colorAttachmentPtr = intptr.alloc colorAttachment

            let mutable renderPassDescriptor =
                RenderPassDescriptor(
                    colorAttachments = colorAttachmentPtr.ptr,
                    colorAttachmentCount = 1u
                )

            let renderPass = encoder.BeginRenderPass &renderPassDescriptor
            renderPass.SetPipeline renderPipeline
            renderPass.Draw(3u, 1u, 0u, 0u)
            renderPass.End()
            let queue = device.GetQueue()
            let mutable commandBufferDescriptor = CommandBufferDescriptor()
            let mutable commandBuffer = encoder.Finish &commandBufferDescriptor
            queue.Submit(1u, &&commandBuffer)
            swapchain.Present()
            window.Poll()
            loop (window.IsClosing()) width height swapchain


    let width, height = window.GetSize()
    let swapchain = recreateSwapchain width height
    printfn "Entering render loop"
    loop (window.IsClosing()) width height swapchain
    device.Drop()
