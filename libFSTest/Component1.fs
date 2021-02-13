namespace libFSTest

open UnityEngine;
open System.Collections.Generic;

module test =
    [<Class>]
    type elm(pos:Vector3) =
        let pos = pos
        let dis = abs pos.x + abs pos.y + abs pos.z

        member this.Dis = dis

    [<Class>]
    type sortedList(v) = 
        let mutable (list:elm[]) = v

        let rec sarch v max min = 
            if max - min = 1
                then min
                else
                    let index = (min + max) / 2
                    printfn "%d %A" index list.[index].Dis
                    if list.[index].Dis = v 
                        then index
                        else
                            if list.[index].Dis > v 
                                then sarch v index min
                                else sarch v max index
        member this.Sarch v = sarch v list.Length 0  





