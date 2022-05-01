module Examples.Window

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Silk.NET.GLFW
open WGPU.Native

type Window(width, height, title) =
    let glfw = GlfwProvider.GLFW.Value

    do
        if not <| glfw.Init() then
            failwith "Failed to initialize GLFW"

    do glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)

    let mutable window =
        glfw.CreateWindow(width, height, title, NativePtr.nullPtr, NativePtr.nullPtr)

    do
        if window = NativePtr.nullPtr then
            glfw.Terminate()
            failwith "Failed to create GLFW window"

    let mutable nativeWindow = GlfwNativeWindow(glfw, window)
    member _.Window = window

    member _.GetSize() =
        let mutable width: int = 0
        let mutable height: int = 0
        glfw.GetWindowSize(window, &width, &height)
        (uint width, uint height)

    member _.IsClosing() = glfw.WindowShouldClose window
    member _.Poll() = glfw.PollEvents()

    member _.GetSurface (instance : Instance) : Surface =
        let mutable surfaceDescriptor =
            printfn "Creating surface descriptor"

            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                let hwnd, _hdc, hinstance = nativeWindow.Win32.Value.ToTuple()

                let mutable info =
                    SurfaceDescriptorFromWindowsHWND(
                        ChainedStruct(sType = SType.SurfaceDescriptorFromWindowsHWND),
                        hinstance,
                        hwnd
                    )

                printfn "Created Windows surface descriptor"
                SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")
            else if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
                let d, w = nativeWindow.X11.Value.ToTuple()

                let mutable info =
                    SurfaceDescriptorFromXlibWindow(
                        ChainedStruct(sType = SType.SurfaceDescriptorFromXlibWindow),
                        d,
                        w.ToUInt32()
                    )

                printfn "Created X11 surface descriptor"
                SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")
            else
                let nw = nativeWindow.Cocoa.Value

                let mutable info =
                    SurfaceDescriptorFromMetalLayer(ChainedStruct(sType = SType.SurfaceDescriptorFromMetalLayer), nw)

                printfn "Created Metal surface descriptor"
                SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")

        printfn "Creating surface from descriptor"
        instance.CreateSurface &surfaceDescriptor

    interface IDisposable with
        member _.Dispose() = glfw.Terminate()
