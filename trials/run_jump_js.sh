echo 'jump_js_* >  jumpfile.txt'
cp jump_js_*.trc Run_50002.trc
cp jump_js_*_ext_forces.mot "Run_500 02_newCOP3.mot"
# add conversion to .trc
opensim-cmd run-tool 2_ik_config.xml
opensim-cmd run-tool run5_id_config.xml
opensim-cmd run-tool run5_analyze_config.xml
opensim-cmd run-tool run5_Setup_RRA_Run_50002_cycle1_v191.xml
opensim-cmd run-tool run5_Setup_CMC_Run_50002_cycle1_v240_probed_0.xml
Rscript jump_CMC_post_processing.r
mv normalized_CMC.csv jump_js_normalized_CMC.csv
rm jump_js # delete the folder and all of its subfolders.
mkdir jump_js
mv RRA_Results_v191_Run_50002_cycle1 jump_js
mv CMC_Results_v240_Run_50002_cycle1_probed_0_muscles_changed_0 jump_js
mv "Run_500 02_newCOP3.mot" jump_js
mv Run_50002.mot Run_50002.trc jump_js
mv Run_50002_analyze__BodyKinematics_acc_global.sto jump_js
mv Run_50002_analyze__BodyKinematics_pos_global.sto jump_js
mv Run_50002_analyze__BodyKinematics_vel_global.sto jump_js
mv Run_50002_body_forces_at_joints.sto jump_js
mv Run_50002_inverse_dynamics.sto jump_js
# pause
