package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 0)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int,
    history_length: Int)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{
  /* not predicting. Change this for your predictor */

  val lht_size: Int = 128  
  val lp_size: Int = 128 



  // pause prediction
  val stall = !io.resp.ready

  // index into the table to get the count
  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> UInt(log2Ceil(coreInstBytes))

  /*BHT Components*/
  var path_history = Reg(
    init = Vec(Seq.fill(1) { UInt("b00", width = history_length) }))

  val local_history_table = Reg( init = Vec(Seq.fill(lht_size) { UInt("b0", width = log2Ceil(lp_size)) }))

  /*//2 bit counter
  val local_prediction = Reg(
    init = Vec(Seq.fill(lp_size) { UInt("b00", width = 2) }))
  */

  //3 bit counter
  val local_prediction = Reg(
    init = Vec(Seq.fill(lp_size) { UInt("b000", width = 3) }))

  val global_prediction = Reg(
    init = Vec(Seq.fill(Math.pow(2, history_length).toInt) { UInt("b11", width = 2) }))

  val choice_prediction = Reg(
    init = Vec(Seq.fill(Math.pow(2, history_length).toInt) { UInt("b00", width = 2) }))

  //end components


  /*Read BHT and make prediction*/
  printf("========================BHT BEGIN==================================\n")
  printf("short pc %b\n", s1_r_idx(log2Ceil(lht_size),0))
  printf("local_history %b\n", local_history_table(s1_r_idx))
  printf("local_prediction %b\n", local_prediction(local_history_table(s1_r_idx)))

  printf("path_history %b\n", path_history(0))
  printf("global_prediction %b\n", global_prediction(path_history(0)))

  val choice = choice_prediction(path_history(0))
  printf("choice %b\n", choice)

  val arbiter_sel = choice(1)
  printf("arbiter_sel %b\n", arbiter_sel)

  val prediction = Mux(arbiter_sel, 
                      global_prediction(path_history(0))(1), 
                      local_prediction(local_history_table(s1_r_idx))(2))

  printf("prediction %b\n", prediction)

  

  // honor defaults
  io.resp.valid := !this.disable_bpd
  // set prediction
  io.resp.bits.takens := prediction
  // tell the pipeline to save 
  io.resp.bits.info := RegNext(arbiter_sel)


  /*Update on commit */

  // on commit, get the index and whether the branch was actually taken
  val valid = this.commit.valid
  val predictor_select = arbiter_sel
  printf("predictor_select %b\n", predictor_select)
  val actual_taken = this.commit.bits.ctrl.taken(0)
  printf("actual_taken %b\n", actual_taken)


  // calculate updated counter value

  val choice_count = choice_prediction(path_history(0))
  printf("choice_count %b\n", choice_count)
  val global_count = global_prediction(path_history(0))
  val local_count = local_prediction(local_history_table(s1_r_idx))





  val lp = local_count(1) 
  printf("lp %b\n", lp) 
  val lp_correct = ~(lp ^ actual_taken) 
  printf("lp_correct %b\n", lp_correct) 
  val gp = global_count(1) 
  printf("gp %b\n", gp) 
  val gp_correct = ~(gp ^ actual_taken)
  printf("gp_correct %b\n", gp_correct) 


  val global_update = Mux(actual_taken,
     Mux(global_count === "b11".U, global_count, global_count + 1.U),
     Mux(global_count === "b00".U, global_count, global_count - 1.U))

  printf("global_update %b\n", global_update)

  /* 2 bit counter
  val local_update = Mux(actual_taken,
     Mux(local_count === "b11".U, local_count, local_count + 1.U),
     Mux(local_count === "b00".U, local_count, local_count - 1.U))
  */
  
  val local_update = Mux(actual_taken,
     Mux(local_count === "b111".U, local_count, local_count + 1.U),
     Mux(local_count === "b000".U, local_count, local_count - 1.U))

  printf("local_update %b\n", local_update)

  val predict_correct = ~(prediction ^ actual_taken)
  printf("predict_correct %b\n", predict_correct)

  val choice_in = Cat(lp_correct, gp_correct)
  printf("choice_in %b\n", choice_in)

  switch(choice_in) {
      is("b00".U){
         printf("local predictor wrong\n")
         printf("global predictor wrong\n")
      }
      is("b01".U){
         printf("local predictor wrong\n")
         printf("global predictor correct\n")
      }
      is("b10".U){
         printf("local predictor right\n")
         printf("global predictor wrong\n")
      }
      is("b11".U){
         printf("local predictor right\n")
         printf("global predictor right\n")
      }
  }


  switch(choice_count) {
      is("b00".U){
          printf("case: 00\n")
          when(choice_in === "b01".U) { 
              choice_prediction(path_history(0)) :=  "b01".U
          }
      }
     is("b01".U){
         printf("case: 01\n")
         when(choice_in === "b10".U) { 
              choice_prediction(path_history(0)) := "b00".U
         }
         when(choice_in === "b01".U) { 
             choice_prediction(path_history(0)) :=  "b10".U
         }
     }
     is("b10".U){
         printf("case: 10\n")
         when(choice_in === "b10".U) { 
             choice_prediction(path_history(0)) :=  "b01".U
         }
         when(choice_in === "b01".U) { 
             choice_prediction(path_history(0)) :=  "b11".U
         }
     }
     is("b11".U){
         printf("case: 11\n")
         when(choice_in === "b10".U) { 
             choice_prediction(path_history(0)) :=  "b10".U
         }
     }
  }

  when (predictor_select) { 
     printf("global predictor selected\n")
 //    global_prediction(path_history(0)) := global_update 
  }

  when (!predictor_select)  { 
    printf("local_predictor_selected\n")
 //   local_prediction(local_history_table(s1_r_idx)) := local_update 
 }
 local_prediction(local_history_table(s1_r_idx)) := local_update 
 global_prediction(path_history(0)) := global_update 

  when(predict_correct) { printf("correct prediction\n") }.otherwise { printf("wrong prediction\n") }
  path_history(0) := this.commit.bits.ctrl.taken(0) | path_history(0) << 1
  local_history_table(s1_r_idx) := this.commit.bits.ctrl.taken(0) | local_history_table(s1_r_idx) << 1

  printf("========================BHT END==================================\n")
  //end update
 
}



