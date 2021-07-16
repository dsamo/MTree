import scala.collection.mutable.{ArrayBuffer, PriorityQueue}
import scala.util.control.Breaks.{break, breakable}

case class MTree[A](var root: Node[A] = new Leaf[A](Vector.empty, null), var M: Int){
    def insert(entry: Entry[A]): Unit ={
        insertRecursion(entry) match {
            case Left(ro) => root = new Branch[A](ro, null);
            case Right(_) =>
        };

        def insertRecursion(entry: Entry[A], curr: Node[A] = root): Either[Vector[RoutingObject[A]], Double] ={
            curr match {
                case current: Leaf[A] =>

                    if (current != root) entry.DistanceFromParent = current.center.distance(entry.center);
                    assert(entry.DistanceFromParent != -1);
                    current.children = current.children :+ entry;
                    if (current.children.length > M) return Left(current.split(M/2));
                    else return Right(entry.DistanceFromParent);

                case current: Branch[A] =>

                    var MinIndex = -1;
                    var MinExpansionIndex = -1;
                    var MinDistance = Double.PositiveInfinity;
                    var MinExpansionDistance = Double.PositiveInfinity;
                    for (i <- 0 to current.children.length - 1) {
                        val CurrentDistance = current.children(i).center.distance(entry.center);
                        val CurrentExpansionDistance = CurrentDistance - current.children(i).radius;
                        if (CurrentDistance <= current.children(i).radius) {
                            if (CurrentDistance < MinDistance){
                                MinDistance = CurrentDistance;
                                MinIndex = i;
                            }
                        }else if (CurrentExpansionDistance < MinExpansionDistance){
                            MinExpansionDistance = CurrentExpansionDistance;
                            MinExpansionIndex = i;
                        }
                    }
                    if (MinIndex == -1) MinIndex = MinExpansionIndex;


                    insertRecursion(entry, current.children(MinIndex).CoveringTree) match {
                        case Left(ro) =>
                            if (current != root) {
                                ro.foreach(e => e.DistanceFromParent = e.center.distance(current.center));
                            }
                            current.children = current.children.take(MinIndex) ++ current.children.drop(MinIndex + 1) ++ ro;
                            var newRadius = -1.0;
                            if (current != root){
                                for (c <- current.children){
                                    if (current.center.distance(c.center) + c.radius > newRadius) newRadius = current.center.distance(c.center) + c.radius;
                                }
                            }
                            if (current.children.length > M) return Left(current.split(M/2));
                            else return Right(newRadius);

                        case Right(value) =>
                            current.children(MinIndex).radius = Math.max(value, current.children(MinIndex).radius);

                            return Right(current.children(MinIndex).radius + current.children(MinIndex).DistanceFromParent);
                    };
            }
        }
    }

    def remove(entry: Entry[A]): Boolean = {
        removeRecursion(entry) match{
            case Left(q) =>
                for (k <- q._1){
                    if (k.isInstanceOf[Leaf[A]]){
                        for (c <- k.asInstanceOf[Leaf[A]].children){
                            this.insert(c);
                        }
                    }else{
                        insertInternal(k);
                    }
                }
                return true;
            case Right(k) =>
                if (k != -1) return true;
                else return false;
        }

        def removeRecursion(entry: Entry[A], radius: Double = -1, curr: Node[A] = root): Either[(ArrayBuffer[Node[A]],Double, Boolean), Double] = {
            if (curr.isInstanceOf[Leaf[A]]){
                val current = curr.asInstanceOf[Leaf[A]];
                var SearchEntry = -1;
                breakable{
                var i=0;
                    for (k <- current.children){
                        if (k.center == entry.center /*&& k.value == entry.value*/) {
                            SearchEntry = i;
                            break;
                        }
                        i = i+1;
                    }
                }

                if (SearchEntry == -1) return Right(-1);

                var deletedDist = current.children(SearchEntry).DistanceFromParent;
                current.children = current.children.take(SearchEntry) ++ current.children.drop(SearchEntry + 1);

                var newRadius = Double.NegativeInfinity;
                if (deletedDist == radius) {
                    for (k <- current.children) {
                        if (k.DistanceFromParent > radius) newRadius = k.DistanceFromParent;
                    }
                }

                if (current.children.length < 2) return Left((ArrayBuffer(current),newRadius, true));
                return Right(newRadius);
            }else {
                val current = curr.asInstanceOf[Branch[A]];
                var i = 0;
                for (c <- current.children) {

                    if (c.center.distance(entry.center) <= c.radius) {
                        removeRecursion(entry, c.radius, c.CoveringTree) match {
                            case Left(q) =>
                                if (!q._2.isNegInfinity) c.radius = q._2;

                                if (q._3) current.children = current.children.take(i) ++ current.children.drop(i + 1);

                                var newRadius = Double.NegativeInfinity;
                                if (q._3 || !q._2.isNegInfinity) {
                                    for (k <- current.children) {
                                        if (k.DistanceFromParent + k.radius > radius) newRadius = k.DistanceFromParent + k.radius;
                                    }
                                }

                                if (current.children.length < 2) {
                                    q._1.append(current);
                                    return Left((q._1, newRadius, true));
                                }

                                return Left((q._1, newRadius, false));

                            case Right(-1) =>

                            case Right(r) =>
                                var newRadius = Double.NegativeInfinity;
                                if (!r.isNegInfinity) {
                                    c.radius = r;

                                    for (k <- current.children){
                                        if (k.DistanceFromParent + k.radius > newRadius) newRadius = k.DistanceFromParent + k.radius;
                                    }
                                }

                                return Right(newRadius);
                        }
                    }
                    i = i + 1;
                }
                return Right(-1);
            }
        }

        def insertInternal(n: Node[A]): Unit ={
            if (n.isInstanceOf[Leaf[A]]){
                for (k <- n.asInstanceOf[Leaf[A]].children){
                    this.insert(k);
                }
            }else{
                n.asInstanceOf[Branch[A]].children.foreach(c => insertInternal(c.CoveringTree))
            }
        }

        return true;
    }

    def update(OldEntry: Entry[A], NewEntry: Entry[A]): Boolean ={
        def updateRecursion(OldEntry: Entry[A], NewEntryValue: A, curr: Node[A] = root): Boolean ={
            if (curr.isInstanceOf[Leaf[A]]){
                val current = curr.asInstanceOf[Leaf[A]];
                for (c <- current.children){
                    if (c.center == OldEntry.center) {
                        c.value = NewEntryValue;
                        return true;
                    }
                }
                return false;
            }else{
                val current = curr.asInstanceOf[Branch[A]];
                var result = -1;
                for (c <- current.children){
                    if (c.center.distance(OldEntry.center) <= c.radius){
                        if (updateRecursion(OldEntry, NewEntryValue, c.CoveringTree)) return true;
                    }
                }
                return false;
            }
        }

        if (OldEntry.center != NewEntry.center){
            if (remove(OldEntry)) {
                insert(NewEntry);
                return true;
            }
            return false;
        }else{
            if (updateRecursion(OldEntry, NewEntry.value)) return true;
            return false;
        }
    }

    def RangeSearch(SearchPoint: Geometry, SearchRadius: Double): Vector[Entry[A]] = {
        def RangeSearchRecursion(SearchPoint: Geometry, SearchRadius: Double, curr: Node[A] = root, DistanceQP: Double = 0): Vector[Entry[A]] = {
            if (curr == root){
                if (curr.isInstanceOf[Leaf[A]]){
                    val current = curr.asInstanceOf[Leaf[A]];
                    var found = Vector[Entry[A]]();
                    for (e <- current.children){
                        if (SearchPoint.distance(e.center) <= SearchRadius){
                            found = found :+ e;
                        }
                    }
                    return found;
                }else{
                    val current = curr.asInstanceOf[Branch[A]];
                    var found = Vector[Entry[A]]();
                    for (or <- current.children){
                        val distanceQP = SearchPoint.distance(or.center);
                        if (distanceQP <= SearchRadius + or.radius) found = found ++ RangeSearchRecursion(SearchPoint, SearchRadius, or.CoveringTree, distanceQP);
                    }
                    return found;
                }
            }else{
                if (curr.isInstanceOf[Leaf[A]]){
                    val current = curr.asInstanceOf[Leaf[A]];
                    var found = Vector[Entry[A]]();
                    for (e <- current.children){
                        if (Math.abs(DistanceQP - e.DistanceFromParent) <= SearchRadius){
                            if (SearchPoint.distance(e.center) <= SearchRadius){
                                found = found :+ e;
                            }
                        }
                    }
                    return found;
                }else{
                    val current = curr.asInstanceOf[Branch[A]];
                    var found = Vector[Entry[A]]();
                    for (or <- current.children){
                        if (Math.abs(DistanceQP - or.DistanceFromParent) <= SearchRadius + or.radius){
                            val distanceQP = SearchPoint.distance(or.center);
                            if (distanceQP <= SearchRadius + or.radius) found = found ++ RangeSearchRecursion(SearchPoint, SearchRadius, or.CoveringTree, distanceQP);
                        }
                    }
                    return found;
                }
            }
        }

        return RangeSearchRecursion(SearchPoint, SearchRadius);
    }

    def kNNSearch(Q: Geometry, k: Int): Array[(Entry[A],Double)] = {
        def NNUpdate(oj: Entry[A], dist: Double, arr: Array[(Entry[A],Double)]): (Array[(Entry[A],Double)],Double) = {
            var NN = arr.sortWith(_._2 < _._2);
            var newDk = 0.0;
            if (k > 1){
                if (dist > NN(NN.length - 2)._2) newDk = dist;
                else newDk = NN(NN.length - 2)._2;
            }else{
                if (dist > NN(NN.length - 1)._2) newDk = dist;
                else newDk = NN(NN.length - 1)._2;
            }

            NN = NN.dropRight(1) :+ (oj,dist);
            return (NN,newDk);
        }
        var PR = new PriorityQueue[(Node[A],Double)]()(Ordering.by((_: (Node[A],Double))._2).reverse);
        PR.enqueue((root,0));
        var NN: Array[(Entry[A],Double)] = Array.fill(k)((null, Double.PositiveInfinity));
        var dk = Double.PositiveInfinity;
        breakable {
            while (!PR.isEmpty) {
                var Current = PR.dequeue();
                if (Current._2 > dk) break;
                var NextNode = Current._1;
                if (NextNode.isInstanceOf[Leaf[A]]) {
                    val current = NextNode.asInstanceOf[Leaf[A]];
                    for (oj <- current.children) {
                        val dist = oj.center.distance(Q);
                        if (dist <= dk) {
                            val (newNN, newDk) = NNUpdate(oj, dist, NN);
                            NN = newNN;
                            dk = newDk;
                        }
                    }
                } else {
                    val current = NextNode.asInstanceOf[Branch[A]];
                    for (or <- current.children) {
                        val dist = or.center.distance(Q);
                        if (dist <= dk + or.radius) {
                            var dmin: Double = 0;
                            if (dist - or.radius > 0) dmin = dist - or.radius;
                            if (dmin <= dk) {
                                PR.enqueue((or.CoveringTree, dmin));
                                val dmax = dist + or.radius;
                                if (dmax < dk) {
                                    val (newNN, newDk) = NNUpdate(null, dmax, NN);
                                    NN = newNN;
                                    dk = newDk;
                                }
                            }
                        }
                    }
                }
            }
        }

        return NN.sortWith(_._2 < _._2);
    }

    def traverse(radius: Double = -1, curr: Node[A] = root): Double ={
        if (curr.isInstanceOf[Leaf[A]]){
            val current = curr.asInstanceOf[Leaf[A]];
            var newRadius = Double.NegativeInfinity;
            var counter = 0;
            var children = 0;
            if (current.children.length < 2) children = children + 1;
            var flag = false;
/*            for (i <- 0 to current.children.length - 1){
                if (current != root){
                    if (radius < current.children(i).DistanceFromParent) {
                        flag = true;
                        counter = counter + 1;
                    }
                }
/*                if (current.children(i).value == "Nani Clows") {
                    println("To vrika");
                    return 1;
                }*/
              //  /*if (current.children(i).DistanceFromParent == -1)*/ println(current.children(i).DistanceFromParent);
            }*/
                if (current != root) {
                    for (k <- current.children) {
                        val CurrentDistance = k.center.distance(current.center);
                        if (CurrentDistance > newRadius) newRadius = CurrentDistance;
                    }
                }
            return newRadius;
        }
        else{
            val current = curr.asInstanceOf[Branch[A]];
            var counter = 0;
            var children = 0;
            if (current.children.length == 1) children = children + 1;
            //if (current.center != null && current.center.x == -4.6484733 && current.center.y == 60.107338) println("To vrika opou nanai")
            //if (current.children.length == 0) println("EXEIS MHDEN PAIDIA");
            var flag = false;
            for (i <- 0 to current.children.length - 1){
                ///*if (current.children(i).DistanceFromParent == -1)*/ println(current.children(i).DistanceFromParent);
                    var newRadius = traverse(current.children(i).radius, current.children(i).CoveringTree);
                    if (newRadius != Double.NegativeInfinity) current.children(i).radius = newRadius;
                    //println("Child: " + i)
            }

            if (current != root) {
                var newRadius = Double.NegativeInfinity;
                for (k <- current.children) {
                    val CurrentDistance = k.center.distance(current.center) + k.radius;
                    if (CurrentDistance > newRadius) newRadius = CurrentDistance;
                }
                return newRadius
            }
            return -1;
        }

    }

    def counter(radius: Double, curr: Node[A] = root): Int = {
        if (curr.isInstanceOf[Leaf[A]]){
            val current = curr.asInstanceOf[Leaf[A]];
            //if (current.children.length == 0) println("EXEIS MHDEN PAIDIA");
            var newRadius = Double.NegativeInfinity;
            var c = 0;
            var children = 0;
            if (current.children.length < 2) children = children + 1;
            var flag = false;
                        for (i <- 0 to current.children.length - 1) {
                            if (current != root) {
                                if (radius < current.children(i).center.distance(current.center)) {
                                    //flag = true;
                                    c = c + 1;
                                    //println("Leaf with center: " + current.center)
                                }
                            }
                            /*                if (current.children(i).value == "Nani Clows") {
                                println("To vrika");
                                return 1;
                            }
                          //  if (current.children(i).DistanceFromParent == -1) println(current.children(i).DistanceFromParent);
                        }*/

                        }
            return c;
        }
        else{
            val current = curr.asInstanceOf[Branch[A]];
            var c = 0;
            var children = 0;
            if (current.children.length == 1) children = children + 1;
            //if (current.center != null && current.center.x == -4.6484733 && current.center.y == 60.107338) println("To vrika opou nanai")
            //if (current.children.length == 0) println("EXEIS MHDEN PAIDIA");
            var flag = false;
            for (i <- 0 to current.children.length - 1){
                if (current!= root && current.center.distance(current.children(i).center) + current.children(i).radius > radius) {
                    c = c + 1
                    //println("Branch with center: " + current.center);
                }
                ///*if (current.children(i).DistanceFromParent == -1)*/ println(current.children(i).DistanceFromParent);
                c = c + counter(current.children(i).radius, current.children(i).CoveringTree);
                //println("Child: " + i)
            }
            return c;
        }
    }

    def find(pt: Entry[A], curr: Node[A] = root): Boolean ={
        if (curr.isInstanceOf[Leaf[A]]){
            /*println("Eftasa sto leaf");*/
            for (k <- curr.asInstanceOf[Leaf[A]].children){
                if (k.center == pt.center && k.value == pt.value){
                    //println("To vrika")
                    return true;
                }
            }
            return false;
        }else{
            var flag = false;
            var i=0
            //println("Level")
            for (k <- curr.asInstanceOf[Branch[A]].children){
/*                if (find(pt, k.CoveringTree)) {
                    println("Child" + i);
                    return true;
                }*/
                //println(k.center.distance(pt.center) <= k.radius);
                if (k.center.distance(pt.center) <= k.radius) {
                   // println("Child: " + i);
                    if (find(pt, k.CoveringTree)) return true;
                }
                i = i+1;
            }
            return false;
        }
    }
}