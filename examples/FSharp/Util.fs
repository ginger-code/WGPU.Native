module Examples.Util
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<IsByRefLike>]
type intptr<'a> =
    struct
        val ptr : nativeint
        new internal ptr = { ptr = ptr }
    end
    static member alloc(item : 'a) =
        let ptr = Marshal.AllocHGlobal(Marshal.SizeOf(item))
        Marshal.StructureToPtr(item, ptr, false)
        new intptr<'a> (ptr)

    interface IDisposable with
        member this.Dispose() = Marshal.FreeHGlobal this.ptr
