(* TODO: Write a translator below. *)

let reg_cnt = ref 0
let label_cnt = ref 1 (* T.dummy_label is 0 *)

let incr : int ref -> int
    =fun cnt ->
      cnt := !cnt + 1;
      !cnt

let rec translate_exp: S.exp -> T.var * T.linstr list
    = fun exp ->
      match exp with
      | S.NUM n ->
          (* Allocate register -- not capped. *)
          let reg = "reg" ^ string_of_int (incr reg_cnt) in
          (reg, [(T.dummy_label, T.COPYC (reg, n))])
      | S.LV lv ->
          (match lv with
          | S.ID id ->
              let reg = "reg" ^ string_of_int (incr reg_cnt) in
              (reg, [(T.dummy_label, T.COPY (reg, id))])
          | S.ARR (id, exp) ->
              let (t1, code) = translate_exp exp in
              let t2 = "reg" ^ string_of_int (incr reg_cnt) in
              (t2, code @ [(T.dummy_label, T.LOAD (t2, (id, t1)))]))
      | S.ADD (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.ADD, t1, t2))])
      | S.SUB (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.SUB, t1, t2))])
      | S.MUL (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.MUL, t1, t2))])
      | S.DIV (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.DIV, t1, t2))])
      | S.MINUS exp ->
          let (t1, code) = translate_exp exp in
          let t2 = "reg" ^ string_of_int (incr reg_cnt) in
          (t2, code @ [(T.dummy_label, T.ASSIGNU (t2, T.MINUS, t1))])
      | S.NOT exp ->
          let (t1, code) = translate_exp exp in
          let t2 = "reg" ^ string_of_int (incr reg_cnt) in
          (t2, code @ [(T.dummy_label, T.ASSIGNU (t2, T.NOT, t1))])
      | S.LT (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.LT, t1, t2))])
      | S.LE (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.LE, t1, t2))])
      | S.GT (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.GT, t1, t2))])
      | S.GE (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.GE, t1, t2))])
      | S.EQ (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.EQ, t1, t2))])
      | S.AND (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.AND, t1, t2))])
      | S.OR (exp1, exp2) ->
          let (t1, code1) = translate_exp exp1 in
          let (t2, code2) = translate_exp exp2 in
          let t3 = "reg" ^ string_of_int (incr reg_cnt) in
          (t3, code1 @ code2 @ [(T.dummy_label, T.ASSIGNV (t3, T.OR, t1, t2))])

and translate_stmt: S.stmt -> T.linstr list
    = fun stmt ->
      match stmt with
      | S.ASSIGN (lv, exp) ->
          (match lv with
          | S.ID id ->
              let (t1, code) = translate_exp exp in
              code @ [( T.dummy_label, T.COPY (id, t1))]
          | S.ARR (id, sz_exp) ->
              let (t1, code1) = translate_exp sz_exp in
              let (t2, code2) = translate_exp exp in
              code1 @ code2 @ [(T.dummy_label, T.STORE ((id, t1), t2))])
      | S.IF (exp, stmt1, stmt2) ->
          let (t1, code1) = translate_exp exp in
          let code_t = translate_stmt stmt1 in
          let code_f = translate_stmt stmt2 in
          let l_t = incr label_cnt in
          let l_f = incr label_cnt in
          let l_x = incr label_cnt in

          code1 @ [(T.dummy_label, T.CJUMP (t1, l_t))] @
          [(T.dummy_label, T.UJUMP (l_f))] @
          [(l_t, T.SKIP)] @ code_t @ [(T.dummy_label, T.UJUMP (l_x))] @
          [(l_f, T.SKIP)] @ code_f @ [(T.dummy_label, T.UJUMP (l_x))] @
          [(l_x, T.SKIP)] 
      | S.WHILE (exp, stmt) ->
          let (t1, code1) = translate_exp exp in
          let code_b = translate_stmt stmt in
          let l_e = incr label_cnt in
          let l_x = incr label_cnt in

          [(l_e, T.SKIP)] @ code1 @
          [(T.dummy_label, T.CJUMPF (t1, l_x))] @ code_b @
          [(T.dummy_label, T.UJUMP l_e)] @ [(l_x, T.SKIP)]
      | S.DOWHILE (stmt, exp) ->
          let (t1, code1) = translate_exp exp in
          let code_b = translate_stmt stmt in
          let l_e = incr label_cnt in
          let l_x = incr label_cnt in

          [(l_e, T.SKIP)] @ code_b @ code1 @
          [(T.dummy_label, T.CJUMPF (t1, l_e))] @
          [(T.dummy_label, T.UJUMP l_x)] @ [(l_x, T.SKIP)]
      | S.READ id ->
          [(T.dummy_label, T.READ id)]
      | S.PRINT exp ->
          let (t1, code1) = translate_exp exp in
          code1 @ [(T.dummy_label, T.WRITE t1)]
      | S.BLOCK block ->
          translate_block block

and translate_stmts: S.stmts -> T.linstr list
    = fun stmts ->
      match stmts with
      | [] -> []
      | stmt::stmts -> ( translate_stmt stmt ) @ ( translate_stmts stmts )

and translate_decl: S.decl -> T.linstr
    = fun decl ->
      match decl with
      | (typ, x) ->
          match typ with
          | S.TINT -> ( T.dummy_label, T.COPYC (x, 0) )
          | S.TARR sz -> ( T.dummy_label, T.ALLOC (x, sz) )

and translate_decls: S.decls -> T.linstr list
    = fun decls ->
      match decls with
      | [] -> []
      | decl::decls -> ( translate_decl decl ) :: ( translate_decls decls )

and translate_block : S.block -> T.linstr list
    = fun block ->
      match block with
      | ( decls, stmts ) -> ( translate_decls decls ) @ ( translate_stmts stmts )

let translate : S.program -> T.program
    =fun s ->
      match s with
      | block -> translate_block block @ [ ( T.dummy_label, T.HALT ) ]
