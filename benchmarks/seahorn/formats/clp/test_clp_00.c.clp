false :- main_verifier_error_split.
verifier_error(0,0,0) :- true.
verifier_error(0,1,1) :- true.
verifier_error(1,0,1) :- true.
verifier_error(1,1,1) :- true.
main_entry :- true.
main_verifier_error(VAR_1,VAR_2) :- (((main_entry,(Main__0_0=0)),(VAR_1=1)),(VAR_2=0)).
main__lr_ph :- (main_entry,((Main__0_0<0);(Main__0_0>0))).
main__bb(VAR_1,VAR_2) :- ((main__lr_ph,(VAR_1=0)),(VAR_2=1)).
main_verifier_error_loopexit(VAR_1,VAR_2) :- (((main__bb(Main__0_i2_0,Main__01_i1_0),(Main__5_0=0)),(VAR_1=(Main__0_i2_0+Main__01_i1_0))),(VAR_2=(Main__0_i2_0+1))).
main__bb(VAR_1,VAR_2) :- (((main__bb(Main__0_i2_0,Main__01_i1_0),((Main__5_0<0);(Main__5_0>0))),(VAR_1=(Main__0_i2_0+1))),(VAR_2=(Main__0_i2_0+Main__01_i1_0))).
main_verifier_error(Main__lcssa8_0,Main__lcssa_0) :- main_verifier_error_loopexit(Main__lcssa8_0,Main__lcssa_0).
main_verifier_error_split :- (main_verifier_error(Main__01_i_lcssa_0,Main__0_i_lcssa_0),(Main__01_i_lcssa_0<Main__0_i_lcssa_0)).
