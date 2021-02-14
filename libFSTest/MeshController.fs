namespace libFSTest.MeshController

open UnityEngine;
open System.Collections.Generic;
open System.Linq;

//こっちはボツになったやつ
module MeshList = 
    // C#とF#でやり取りするところ
    [<Class>]
    type V3(a,b,c) = 
        member this.v = Vector3(a,b,c)
        member this.x = a
        member this.y = b
        member this.z = c


    [<Class>]
    type Triangle(a:V3,b:V3,c:V3) =

        let Manhattandistance (vec:Vector3) = vec.x + vec.y + vec.z

        let mutable v = Manhattandistance (a.v + b.v + c.v)
        

        member this.a = a
        member this.b = b
        member this.c = c
        member this.value = v

        member this.CenterOfGravity = (a.v + b.v + c.v) / 3.0f
        member this.CenterV3 = this.CenterOfGravity |> fun x -> V3(x.x,x.y,x.z)
        member this.ScreenPos = Camera.main.WorldToViewportPoint this.CenterOfGravity
        member this.PixPos = Camera.main.WorldToScreenPoint this.CenterOfGravity

        member this.Compair (v:Triangle) = 
            a.v = v.a.v && v.b.v = b.v && c.v = v.c.v

        member this.Normal = 
            ((c.v - a.v),(b.v - a.v))
            |> fun (x,y) -> Vector3.Cross(y,x)
            |> fun x -> Vector3.Normalize x
            |> fun x -> V3(x.x,x.y,x.z)


    [<Class>] 
    type TriangleList() =
        // 3つのベクトルの３頂点の各成分の和　で常にソートされた重複のない配列
        // 1つの値を加えるのに　O(logn)　見つけるのに　O(logn)
        let mutable (sorted:Triangle[]) = [||]

        //O(logN)
        // 3つのベクトルの３頂点の各成分の和が等しいものは存在しないものとする
        // ↑等しいものもそのうち出るはず
        let rec find (v:Triangle) max min = 
            let middle = (max + min) / 2
            match max-min with
            | 0 -> if (sorted.[middle].value - v.value) > 0.0f
                        then min - 1
                        else min
            | 1 -> if (sorted.[min].value - v.value) > 0.0f 
                    then    min-1
                    else    find v max max
            | _ -> if sorted.[middle].Compair v
                    then
                         middle
                    else 
                        if (sorted.[middle].value - v.value) > 0.0f
                            then
                                find v middle min
                            else
                                find v max middle
                          
        member this.Compair v = 
            let index = find v (sorted.Length-1) 0
            v = sorted.[index]

        member this.AddTriangle v = 
            if sorted.Length = 0 
                then 
                    sorted <- [|v|]
                else         
                    let index = find v (sorted.Length-1) 0
                    if index = -1 
                        then
                            sorted <- [| for i in 0 .. sorted.Length -> 
                                              if i = 0
                                                  then v 
                                                  else sorted.[i-1] |]
                        else
                        if sorted.[index].Compair v = false
                          then
                              sorted <- [| for i in 0 .. sorted.Length -> 
                                                if i <= index 
                                                    then sorted.[i] 
                                                    else 
                                                        if i-1 = index     
                                                            then v 
                                                            else sorted.[i-1] |]

                                                           

        member this.Add a b c = 
            let v = (Triangle(a,b,c))
            this.AddTriangle v

        member this.Triangles = Array.copy sorted

        member this.Clear = 
            sorted <- [||]

        member this.Set v = sorted <- v //強制的に入れるので基本使わない

        member this.list = 
         [| for i in 0 .. 3*(sorted.Length)-1
                 -> match i%3 with 
                    | 1 -> sorted.[i/3].b
                    | 2 -> sorted.[i/3].c
                    | _ -> sorted.[i/3].a |]

        member this.makeTriangles = 
            [| 0 .. 3*(sorted.Length)-1|]

        member this.Find a b c = 
            find (Triangle(a,b,c))

    [<Class>]
        type Screenedriangles() = 
            let mutable l = TriangleList()
            let mutable selected = TriangleList()

            member this.Add (v:Triangle) depth =   
                if v.ScreenPos.x < 1.0f && v.ScreenPos.x > 0.0f && v.ScreenPos.y < 1.0f && v.ScreenPos.y > 0.0f
                    then 
                        if (v.CenterOfGravity.z * 100.0f) |> int = (depth * 100.0f |> int)
                            then l.AddTriangle v;

            member  this.Clear b = 
                if b = false then selected.Clear

            member this.Select v = 
                selected.AddTriangle v

            member this.SelectRange (a:Vector2) (b:Vector2) = 
                let left = Vector2(min a.x b.x,min a.y b.y)
                let right = Vector2(max a.x b.x,max a.y b.y)
                l.Triangles
                 |> Array.filter (fun elm -> elm.PixPos.x >= left.x && elm.PixPos.y >= left.y )
                 |> Array.filter (fun elm -> elm.PixPos.x <= right.x && elm.PixPos.y <= right.y )
                 |> Array.map (fun x -> selected.AddTriangle x)
                 
            member this.lLength = l.Triangles.Length
            member this.sLength = selected.Triangles.Length;
            member this.L = l

            member this.Seleted = selected
            member this.Mesh = selected.list
            member this.Triangles = selected.makeTriangles

//ここから使うやつ
module Polygon = 
    //循環する場合andで書けばできる
    type Vertex ={
            position: Vector3
            index: int
        }
    and HalfEdge =
        class
            val mutable vert: int
            val mutable next: int
            val mutable prev: int
            val mutable pair: Adress
            val face: int 
            new(v,n,p,pa,f) = {vert = v;next = n;prev = p; pair = pa;face = f}

            //ベジェ曲線とかやるときのための
            abstract member Type : string
            default this.Type = "HalfEdge"
            member this.NextUpdate n = this.next <- n
            member this.PrevUpdate n = this.prev <- n

        end

    and Face =
        class
            val mutable halfEdges: List<HalfEdge>
            val start: int
            val num: int 
            val mutable count: int
            val mutable Normal : Vector3

            new(n) = { halfEdges = List<HalfEdge>();start = 0;num = n;count = 0; Normal = Vector3.zero }

            member this.SetNormal v = this.Normal <- v

            //３角形に分割
            abstract member Triangles : List<int>

            //最初と現在の辺の前と後ろをつなげて３角形に
            default this.Triangles = 
                let mutable l = List<int>()
                let mutable index = this.start
                let mutable prev = this.halfEdges.Item(index).next
                index <- this.halfEdges.Item(prev).next
                while index <> this.start do
                    this.halfEdges.Item(this.start).vert
                    |> l.Add
                    this.halfEdges.Item(index).vert
                    |> l.Add
                    this.halfEdges.Item(prev).vert
                    |> l.Add
                    prev <- index
                    index <- this.halfEdges.Item(index).next
                l

            //辺を出力するための関数
            //listの長さ　2(n-1)
            member this.sarchEdge =
                let mutable l = List<int>()
                let mutable index = this.start
                this.halfEdges.Item(index).vert
                    |> l.Add
                index <- this.halfEdges.Item(index).next
                this.halfEdges.Item(index).vert
                    |> l.Add
                while index <> this.start do
                    this.halfEdges.Item(index).vert
                        |> l.Add
                    index <- this.halfEdges.Item(index).next
                    this.halfEdges.Item(index).vert
                        |> l.Add
                l

            //辺を巡回しながら引数に入れた関数を実行していく
            //入れる関数は頂点番号を引数に要求するもの
            //引数に関数を入れるというのを理解するの時間がかかったので一部でしか使えていない
            member this.sarchF func = 
                let mutable index = this.start
                func index
                index <- this.halfEdges.[index].next
                while index <> this.start do
                    func index
                    index <- this.halfEdges.[index].next

            //辺と辺の間に新しい辺を挿入する
            member this.Insert p v = 
               match this.count with
                | 0 -> 
                    this.halfEdges.Add <| HalfEdge(v,0,0,{x = this.num;y = this.count},this.num)
                    this.count <- 1
                | 1 ->
                    this.halfEdges.Add <| HalfEdge(v,0,0,{x = this.num;y = this.count},this.num)
                    this.halfEdges.Item(0).NextUpdate 1
                    this.halfEdges.Item(0).PrevUpdate 1
                    this.count <- 2
                | _ -> //メッシュが作れるようになったら前のハーフエッジのnextを新しいものに　次のprevを新しいものに
                   let prevEdge = this.halfEdges.Item (p)
                   let nextNum = prevEdge.next
                   let nextEdge = this.halfEdges.Item (nextNum)
                   this.halfEdges.Add <| HalfEdge(v,nextNum,p,{x = this.num;y = this.count},this.num)
                   prevEdge.NextUpdate this.count
                   nextEdge.PrevUpdate this.count
                   this.count <- this.count+1
               
       end
    and Mesh = 
        class
            val mutable faces: List<Face>
            val mutable faceCount: int
            val mutable verticies: List<Vertex>
            val mutable vertics: List<Vector3>
            val mutable verticiesCount: int
            val mutable triangleindecies : List<int>
            new() = {faces = List<Face>();faceCount = 0;verticies = List<Vertex>();vertics = List<Vector3>();verticiesCount = 0; triangleindecies = List<int>()}

            //法線が求められてないとき
            //面の最初の3つのへんの外積を求めて法線を求める
            member this.Normal index = 
                if this.faces.[index].Normal = Vector3.zero
                    then
                        let f = this.faces.Item(index)
                        let aHE = f.halfEdges.[f.start]
                        let a = this.vertics.[aHE.vert]
                        let bHE = f.halfEdges.[aHE.next]
                        let b = this.vertics.[bHE.vert]
                        let cHE = f.halfEdges.[bHE.next]
                        let c = this.vertics.[cHE.vert]

                        Vector3.Cross((a-b), (c-b))
                        |> Vector3.Normalize
                        |> this.faces.[index].SetNormal
                this.faces.[index].Normal

            member this.AddNewFace () = 
                this.faces.Add <| Face(this.faceCount)
                this.faceCount <- 1 + this.faceCount

            member this.AddVertex p = 
                {position = p;index = this.verticiesCount}
                |> this.verticies.Add
                this.vertics.Add p
                this.verticiesCount <- 1 + this.verticiesCount

            member this.AddEdge faceNum p v = 
                this.faces.Item(faceNum).Insert p v

            //最初のへんの前を見て辺を追加
            member this.AddEdgeTail faceNum v =
                let item = this.faces.Item(faceNum)
                let p = if item.count = 0 then 0 else item.halfEdges.Item(0).prev
                this.faces.Item(faceNum).Insert p v

            // 面をコピーする
            // 回る順序は　func　できまる
            member this.CopyFace faceNum (func : HalfEdge -> HalfEdge) = 
                this.AddNewFace ()
                let originFace = this.faces.Item(faceNum)
                let newFace = this.faces.Item(this.faceCount-1)
                //下の２つでセット
                //頂点をコピーしてコピーしたものを変として追加 (プログラムは逆で行っている)
                this.verticiesCount
                    |> fun x -> this.AddEdgeTail newFace.num x
                originFace.start
                    |> fun x -> originFace.halfEdges.Item(x).vert
                    |> fun x -> this.verticies.[x].position
                    |> this.AddVertex
                let startEdge = 
                    originFace.start
                    |> fun x -> originFace.halfEdges.Item(x)
                let  mutable next = func startEdge
                while next <> startEdge do
                    this.verticiesCount
                        |> fun x -> this.AddEdgeTail newFace.num x
                    next
                        |> fun x -> x.vert
                        |> fun x -> this.verticies.[x].position
                        |> this.AddVertex
                    next <- func next

            //角柱を作るときの底面
            //使ってない　上のCopyをやる前に作ったもの
            member this.MakeBottom faceNum =
                this.AddNewFace ()
                let f = this.faces.Item(faceNum)
                let bottom = this.faces.Item(this.faceCount-1)
                this.verticiesCount
                    |> fun x -> this.AddEdgeTail bottom.num x
                f.start
                    |> fun x -> f.halfEdges.Item(x).vert
                    |> fun x -> this.verticies.[x].position
                    |> this.AddVertex
                let mutable prev =          //反対周りにするので一つ前を参照する
                    f.halfEdges.Item(f.start).prev
                while prev <> f.start do
                    this.verticiesCount
                        |> fun x -> this.AddEdgeTail bottom.num x
                    prev
                        |> fun x -> f.halfEdges.Item(x).vert
                        |> fun x -> this.verticies.[x].position
                        |> this.AddVertex
                    prev <- f.halfEdges.Item(prev).prev
                
            member this.MakeSide adress bottomAdress= 
                let f = this.faces.Item(adress.x)
                let bottomF = this.faces.Item( bottomAdress.x)
                let edge = f.halfEdges.Item(adress.y)
                let bottomEdge = bottomF.halfEdges.Item(bottomAdress.y)
                let vertex_1 = edge.vert
                let vertex_2 = f.halfEdges.Item(edge.next).vert
                let vertex_3 = bottomEdge.vert
                let vertex_4 = bottomF.halfEdges.Item(bottomEdge.prev).vert
                this.AddNewFace ()
                this.AddEdgeTail (this.faceCount-1) vertex_1
                this.AddEdgeTail (this.faceCount-1) vertex_3
                this.pair bottomAdress {x = this.faceCount-1;y = 1}
                this.AddEdgeTail (this.faceCount-1) vertex_4
                this.AddEdgeTail (this.faceCount-1) vertex_2

            member this.Connect adress bottomAdress= 
                let f = this.faces.Item(adress.x)
                let bottomF = this.faces.Item( bottomAdress.x)
                let edge = f.halfEdges.Item(adress.y)
                let bottomEdge = bottomF.halfEdges.Item(bottomAdress.y)
                let vertex_1 = edge.vert
                let vertex_2 = f.halfEdges.Item(edge.next).vert
                let vertex_3 = bottomEdge.vert
                let vertex_4 = bottomF.halfEdges.Item(bottomEdge.next).vert
                this.AddNewFace ()
                this.AddEdgeTail (this.faceCount-1) vertex_1
                this.AddEdgeTail (this.faceCount-1) vertex_4
                this.AddEdgeTail (this.faceCount-1) vertex_3
                this.AddEdgeTail (this.faceCount-1) vertex_2

            //角柱を作る
            member this.MakePillar index = 
                let Origin = this.faces.Item(index)
                //一つ前に遡る関数　HalfEdge -> HalfEdge
                let prev (face:Face) (edge:HalfEdge) = 
                    edge.prev 
                        |> fun x ->  face.halfEdges.Item(x)
                //反対周りにコピーする
                this.CopyFace index (prev Origin)
                let f = this.faces.Item(index)
                let bottomFace = this.faces.Item(this.faceCount-1)
                this.MakeSide {x = index;y = f.start} {x = bottomFace.num; y = bottomFace.start}
                let mutable next = f.halfEdges.[f.start].next
                let mutable prev = bottomFace.halfEdges.[bottomFace.start].prev
                while next <> f.start do
                    this.MakeSide {x = index;y = next} {x = bottomFace.num; y = prev}
                    next <- f.halfEdges.[next].next
                    prev <-bottomFace.halfEdges.[prev].prev

            //面を押し出すため側面を作る
            member this.Push index = 
                let Origin = this.faces.Item(index)
                let mutable edge = 
                    Origin.start 
                    |> fun x -> Origin.halfEdges.Item(x)
                //頂点をコピー
                edge.vert
                    |> fun x -> this.vertics.Item(x)
                    |> this.AddVertex
                let mutable originPrevVert = edge.vert
                let startVert = edge.vert //最後に使う
                //頂点を更新
                edge.vert <- this.vertics.Count - 1
                while Origin.start <> edge.next  do
                    let next = 
                        edge.next
                            |> fun x -> Origin.halfEdges.Item(x)
                    next.vert
                        |> fun x -> this.vertics.Item(x)
                        |> this.AddVertex
                    this.AddNewFace()
                    let newF = 
                        this.faces.Count-1
                            |> fun x -> this.faces.Item(x)
                    //側面を作る
                    this.AddEdgeTail newF.num originPrevVert
                    this.AddEdgeTail newF.num next.vert
                    this.AddEdgeTail newF.num (this.vertics.Count - 1)
                    this.AddEdgeTail newF.num edge.vert
                    edge <- next
                    originPrevVert <- edge.vert
                    edge.vert <- this.vertics.Count - 1
                //最後の面
                let next = 
                    edge.next
                        |> fun x -> Origin.halfEdges.Item(x)
                this.AddNewFace()
                let newF = 
                    this.faces.Count-1
                        |> fun x -> this.faces.Item(x)
                this.AddEdgeTail newF.num originPrevVert
                this.AddEdgeTail newF.num startVert
                this.AddEdgeTail newF.num next.vert
                this.AddEdgeTail newF.num edge.vert


            member this.FaceTranslation faceIndex scalar =
                let translation (face:Face) index = //与えられた頂点インデックスの頂点を
                    face.halfEdges.Item(index).vert //法線のスカラー倍をたす　関数
                    |> fun x -> this.vertics.Item(x) <- this.vertics.Item(x) + (this.Normal faceIndex) * scalar
                let f = this.faces.Item faceIndex
                f.sarchF <| translation f

            member this.SarchEdge () = 
                let mutable list = List<int>()
                for item in this.faces do
                    list.AddRange(item.sarchEdge)
                list

            //三角形に分割して　三角形がどこの面に属するか割り振る
            member this.Triangles () =
                let mutable list = List<int>()
                this.triangleindecies <- List<int>()
                for item in this.faces do
                    let prevIndex = list.Count
                    list.AddRange(item.Triangles)
                    if prevIndex <> list.Count
                        then 
                            let count = (list.Count - prevIndex) / 3
                            for i in 1..count do
                                this.triangleindecies.Add(item.num)
                list
            member this.Sarchvertex ()= 
                let debugVert (face:Face) index =
                    face.halfEdges.Item(index).vert
                    |>fun x ->Debug.Log x;this.vertics.[x]
                    |>Debug.Log
                for item in this.faces do
                    Debug.Log ("start" + item.num.ToString())
                    item.sarchF <| debugVert item

            //各頂点をDebug.Log
            member this.SarchVert index =
                let debugVert (face:Face) index =
                    face.halfEdges.Item(index).vert
                    |>fun x ->Debug.Log x;this.vertics.[x]
                    |>Debug.Log
                this.faces.[index].sarchF <| debugVert this.faces.[index]

            member this.pair adress_1 adress_2 =
                    this.faces.Item(adress_1.x)
                        |> fun x -> x.halfEdges.Item(adress_1.y)
                        |> fun x -> x.pair <- adress_2
                    this.faces.Item(adress_2.x)
                        |> fun x -> x.halfEdges.Item(adress_2.y)
                        |> fun x -> x.pair <- adress_1
       
        end
    and Adress =
        {   x: int
            y: int
        }

        member this.X = this.x
        member this.Y = this.y


    //ここからは使用してない
    and BezierCurve =
       inherit HalfEdge

       val ankerpoint: Vector3
       val smoothness: int
       val endpoint: int //普通のハーフエッジにおけるnext
       new (v,n,p,pa,f,ap,e) = {inherit HalfEdge(v,n,p,pa,f);ankerpoint = ap;smoothness = 8;endpoint = e }
       new (v,n,p,pa,f,ap,smooth,e) = {inherit HalfEdge(v,n,p,pa,f);ankerpoint = ap;smoothness = smooth;endpoint = e}

       override this.Type = "bezierCurve"

    and BezierCurveFace = //ベジェ曲線のところの側面
        inherit Face
        val mutable straightLine:int //直線のところを一つ startの反対側

        new(n) = { inherit Face(n);straightLine = 0 }
        override this.Triangles = 
            let mutable v = List<int>()
            let mutable p_1_edge = this.halfEdges.Item(this.straightLine)
            let mutable p_1 = p_1_edge.vert
            let mutable p_2_edge = p_1_edge.next |> (fun x -> this.halfEdges.Item(x))
            let mutable p_2 = p_2_edge.vert
            let mutable p_3_edge = p_2_edge.next |> (fun x -> this.halfEdges.Item(x))
            let mutable p_3 = p_3_edge.vert
            let mutable p_4_edge = p_1_edge.prev |> (fun x -> this.halfEdges.Item(x))
            let mutable p_4 = p_4_edge.vert
            p_1 |> v.Add
            p_2 |> v.Add
            p_3 |> v.Add
            p_1 |> v.Add
            p_3 |> v.Add
            p_4 |> v.Add
            while p_3 = this.halfEdges.Item(this.start).vert do
                p_1_edge <- p_4_edge
                p_1 <- p_1_edge.vert
                p_2_edge <- p_3_edge
                p_2 <- p_2_edge.vert
                p_3_edge <- p_3_edge.next |> (fun x -> this.halfEdges.Item(x))
                p_3 <- p_3_edge.vert
                p_4_edge <- p_1_edge.prev |> (fun x -> this.halfEdges.Item(x))
                p_4 <- p_4_edge.vert
                p_1 |> v.Add
                p_2 |> v.Add
                p_3 |> v.Add
                p_1 |> v.Add
                p_3 |> v.Add
                p_4 |> v.Add
            v
