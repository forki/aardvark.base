
namespace Aardvark.Base


open System
open System.Threading
open System.Collections.Generic

type NativeCall = nativeint * obj[]

type CodeId = int


[<AllowNullLiteral>]
type ICodeFragment =
    abstract member AppendId : unit -> CodeId
    abstract member Update : CodeId * seq<NativeCall> -> unit
    abstract member Clear : unit -> unit


type IDynamicProgram<'f when 'f :> ICodeFragment> =
    abstract member Prolog : 'f
    abstract member Epilog : 'f

    abstract member NewAfter : 'f -> 'f
    abstract member NewBefore : 'f -> 'f

    abstract member Remove : 'f -> bool
    abstract member Run : unit -> unit



[<AllowNullLiteral>]
type CodeFragment(manager : MemoryManager<unit>) =
    static let encodedJumpSize = 8
    
    let mutable ptr : managedptr<unit> = null
    let mutable prev : CodeFragment = null
    let mutable next : CodeFragment = null
    
    let mutable dirty = true
    let calls = List()

    let writeJumpArgument() =
        if dirty then 
            ()
        else 
            let jumpArgumentOffset = ptr.Size - encodedJumpSize + 1
            let jumpArgumentOffset = (jumpArgumentOffset + 3) &&& ~~~3 // align the argument to 4 bytes

            if not (isNull next) then
                let arg = int (next.Offset - (ptr.Offset + nativeint (jumpArgumentOffset + 4)))
                ptr.Write(jumpArgumentOffset, arg)

    member x.WriteJump() =
        if dirty then x.Write()
        else writeJumpArgument()


    member private x.Dirty 
        with get() = dirty
        and set d = dirty <- d

    member x.Offset = 
        if isNull ptr then 0n
        else ptr.Offset

    member x.Next
        with get() = next
        and set n = 
            if next <> n then
                next <- n
                writeJumpArgument()

    member x.Prev
        with get() = prev
        and set p = prev <- p 

    member x.AppendId() =
        let id = calls.Count
        calls.Add []
        id

    member x.Update(id : int, code : seq<NativeCall>) =
        let code = Seq.toList code
        if code <> calls.[id] then
            calls.[id] <- code
            dirty <- true

    member x.Clear() =
        if calls.Count > 0 then
            calls.Clear()
            dirty <- true

    member x.Write() =
        if dirty then
            dirty <- false
            let calls = calls |> Seq.toList |> List.collect id
            let binary = calls |> Assembler.compileCalls 0
            let size = binary.Length + encodedJumpSize

            let jumpArgumentOffset = (binary.Length + 1)
            let jumpArgumentOffset = (jumpArgumentOffset + 3) &&& ~~~3 // align the argument to 4 bytes
            let jumpOffset = jumpArgumentOffset - 1

            // allocate if not already done
            if isNull ptr then
                ptr <- manager |> MemoryManager.alloc () size
                // write the nops before
                let nopsBefore = jumpOffset - binary.Length
                if nopsBefore > 0 then
                    ptr |> ManagedPtr.writeArray binary.Length (Array.create nopsBefore 0x90uy)


                // write the instruction itself
                ptr |> ManagedPtr.write jumpOffset 0xFFuy
                

            // realloc if non-matching size
            elif ptr.Size <> size then
                if ptr |> ManagedPtr.realloc size then
                    if not (isNull prev) then 
                        prev.Dirty <- true

                // write the nops before
                let nopsBefore = jumpOffset - binary.Length
                if nopsBefore > 0 then
                    ptr |> ManagedPtr.writeArray binary.Length (Array.create nopsBefore 0x90uy)


                // write the instruction itself
                ptr |> ManagedPtr.write jumpOffset 0xFFuy    
                      
            // write the content    
            ptr.Write(0, binary)

            writeJumpArgument()

            // write prev's jump
            if not (isNull prev) then
                prev.WriteJump()

[<AllowNullLiteral>]
type CodeFragmentNew(manager : MemoryManager<CodeFragmentNew>) as this =
    static let encodedJumpSize = 8

    static let ret = Assembler.functionEpilog 6

    let mutable containsJmp = false
    let mutable ptr = manager.Alloc(encodedJumpSize, this)
    let mutable next : CodeFragmentNew = null
    let mutable prev : CodeFragmentNew = null

    let startOffsets = List [0]

    let writeJmp() =
        let encodedJumpStart = ptr.Size - encodedJumpSize
        if not (isNull next) then
            let jumpArgumentStart = (encodedJumpStart + 1 + 3) &&& ~~~3
            let jumpStart = jumpArgumentStart - 1
            
            let arg = next.Offset - ptr.Offset - nativeint (jumpStart + 5)
            ptr |> ManagedPtr.writeArray encodedJumpStart (Array.create (jumpStart - encodedJumpStart) 0x90uy)
            ptr |> ManagedPtr.write jumpStart 0xE9uy
            ptr |> ManagedPtr.write jumpArgumentStart (int arg)

        else
            ptr |> ManagedPtr.writeArray encodedJumpStart ret

    do writeJmp()

    member private x.WriteJump() =
        writeJmp()


    member x.Next
        with get() = next
        and set n = 
            next <- n
            writeJmp()

    member x.Prev 
        with get() = prev
        and set p = prev <- p

    member x.NextMemoryOrder =
        if isNull ptr.Next then null
        else ptr.Next.Tag

    member x.PrevMemoryOrder =
        if isNull ptr.Prev then null
        else ptr.Prev.Tag


    member x.Size =
        ptr.Size - encodedJumpSize

    member x.End =
        ptr.Offset + nativeint ptr.Size

    member x.Offset =
        ptr.Offset

    member x.Realloc(size : int) =
        let moved = ptr |> ManagedPtr.realloc (size + encodedJumpSize)

        writeJmp()

        if moved && not (isNull prev) then
            prev.WriteJump()


    member x.Append(data : byte[]) =
        let id = startOffsets.Count - 1
        let offset = startOffsets.[id]

        x.Realloc(x.Size + data.Length)

        ptr |> ManagedPtr.writeArray offset data
        startOffsets.Add (offset + data.Length)


        id

    member x.Update(id : int, data : byte[]) =
        let start = startOffsets.[id]
        let nextStart = startOffsets.[id+1]
        let length = nextStart - start
        let sizeDelta = data.Length - length

        if sizeDelta < 0 then
            ptr |> ManagedPtr.move nextStart (start + data.Length) (x.Size - nextStart)
            ptr |> ManagedPtr.writeArray start data
            x.Realloc(x.Size + sizeDelta)

        elif sizeDelta > 0 then
            x.Realloc(x.Size + sizeDelta)
            ptr |> ManagedPtr.move nextStart (start + data.Length) (x.Size - nextStart)
            ptr |> ManagedPtr.writeArray start data

        if sizeDelta <> 0 then
            for i in id+1..startOffsets.Count-1 do
                startOffsets.[i] <- startOffsets.[i] + sizeDelta

    member x.Clear() =
        x.Realloc(0)
        
        if startOffsets.Count > 1 then
            startOffsets.RemoveRange(1, startOffsets.Count-1)

        startOffsets.[0] <- 0

    member x.MakeJumpLocal() =
        if not (isNull next) && x.End <> next.Offset then
            let nextSize = next.Size + encodedJumpSize
            let mutable collected = 0
            let mutable currentPtr = ptr.Next
            let ptrs = List<_>()

            while collected < nextSize do
                if currentPtr.Free then
                    manager.Alloc currentPtr
                else
                    let f = currentPtr.Tag
                    let oldPtr = manager.Spill currentPtr

                    f.WriteJump()
                    if not (isNull f.Prev) then f.Prev.WriteJump()

                collected <- collected + currentPtr.Size
                ptrs.Add currentPtr


            let newPtr = manager.Merge ptrs
            
            if newPtr.Size > nextSize then
                newPtr |> ManagedPtr.realloc nextSize |> ignore

            //copy everything from next to newPtr


            ()
                







            



[<AutoOpen>]
module ICodeFragmentExtensions =

    type ICodeFragment with
        
        member x.Append(calls : seq<NativeCall>) =
            let id = x.AppendId()
            x.Update(id, calls)
            id

        member x.Clear(id : CodeId) =
            x.Update(id, Seq.empty)

