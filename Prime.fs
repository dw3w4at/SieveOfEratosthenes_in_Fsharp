module Prime =
  let primeTable = ref (Seq.empty<int64>)
  let takeSmallerThanSquare n sequence =
    Seq.takeWhile (fun elem -> elem * elem <= n) sequence
  let existsMultiple n sequence =
    Seq.exists (fun elem -> n % elem = 0L) sequence
  let isPrimeElement elem =
    match (existsMultiple elem <| takeSmallerThanSquare elem !primeTable) with
    | true -> None
    | false -> primeTable := Seq.append !primeTable (Seq.singleton elem);Some(elem)
  let seedSeq = Seq.append
                <| Seq.ofList [2L; 3L; 5L; 7L]
                <| Seq.unfold
                     (fun (current, next) ->
                       match next % 10L with
                       | 3L -> Some(current, (next, next + 4L))
                       | _ -> Some(current, (next, next + 2L))
                     ) (11L, 13L)
  let sieveOfEratosthenes = Seq.choose isPrimeElement seedSeq
 (*let sieveOfEratosthenes = seq {
    let primeTable = ref (Seq.ofList [2L ;3L ;5L ;7L])
    let sieve'sSeed = [11L; 13L; 17L; 19L]
    let takeSmallerThanSquare n sequence =
      Seq.takeWhile (fun elem -> elem * elem <= n) sequence
    let existsMultiple n sequence =
      Seq.exists (fun elem -> n % elem = 0L) sequence
    let sieve digit =
      let isPrimeElement seed =
        let elem = digit * 10L + seed
        match (existsMultiple elem <| takeSmallerThanSquare elem !primeTable) with
        | true -> None
        | false -> Some(elem)
      primeTable := Seq.append !primeTable (Seq.choose isPrimeElement sieve'sSeed)
    List.iter (fun i -> sieve i) [0L..500L]
    yield! !primeTable
  };;*)
Seq.last Prime.sieveOfEratosthenes;;