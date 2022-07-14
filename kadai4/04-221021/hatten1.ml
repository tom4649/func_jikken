(*reader monadの実装*)
type ('a,'b) reader = Reader of ('a -> 'b) (*reader型:環境を受け取って値を返す関数,型'aには環境の型を入れる*)
(*type 'a m = (env,'a)reader モナドの型構成子*)
let run (Reader r) = r (*reader型を関数に変える関数*)
let  (>>=) m f = Reader (fun env -> (run (f (run m env))) env)(*計算をつなげる*)
let return x = Reader (fun _ -> x) (*reader型を返す*)
let ask () = Reader(fun env -> env) (*環境をそのまま返すreader型*)
let local f m = Reader(fun env -> run m (f env))(*環境を操作する関数を受け取って一時的に環境を書き換える関数*)
let map f m = Reader (fun env -> f (run m env))(*Reader型のmが表す関数の返り値にfの操作を行う関数*)

(*(*以下動作例*)
type myenv = {month:int;date:int;weather:string}
type 'a m = (myenv,'a)reader
let to_string_info month date weather = (string_of_int month)^"/"^(string_of_int date)^" weather:"^weather
let get_weather env = env.weather
let message env m = run m env 
let _ =
  let env' = {month = 1;date = 1 ; weather = "sunny" } in
  let res = 
    return 5
    |> map (fun x -> x+14)
    >>= (fun x -> (ask() |>  map get_weather |> map (to_string_info 5 x)) )
    |> local (fun env -> {env with weather = "rainy"})
    |> message env' in 
  Printf.printf "%s\n" res;;

(*モナド則を満たすことの確認*)
let m = return 1
let f = fun x -> ask() |> map(fun env -> x+1)
let g = fun x -> ask() |> map(fun env -> x+10)
let testenv = {month =1;date=1;weather="cloudy"}
let p1 = run (return 1 >>= f) testenv= run (f 1)testenv
let p2 = run (m >>= return) testenv =run m testenv
let p3 = run ((m >>= f) >>= g) testenv= run (m >>= (fun x -> f x >>= g)) testenv
*)