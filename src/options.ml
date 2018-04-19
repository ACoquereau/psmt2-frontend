let verbose = ref false
let status = ref "undef"
    
let set_verbose b = verbose := b

let set_status s = status := s

let verbose () = !verbose

let status () = !status


