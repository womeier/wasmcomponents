(** * Instantiating a component's core modules

    The components this project ships contain two core modules — the one
    CertiRocq's Wasm backend emits and the adapter that wraps it in the
    canonical ABI — and wire them together inside the component:

    <<
      (core instance $b (instantiate $backend))
      (core instance $a (instantiate $adapter (with "rocq" (instance $b))))
    >>

    This file gives that wiring a semantics.  It is what stands in for a
    static link.

    ** Why not link

    The alternative is [wasm-merge]: produce one core module containing both
    halves, with every index space concatenated and renumbered.  Two problems.
    The renumbering is binaryen's to choose and nothing pins it — on the
    [id63] example it preserves the backend's function indices but *permutes
    its globals*, so a proof that names [glob_out_of_mem] by a numeral is
    talking about a module that may not be the artefact.  And binaryen is an
    unverified C++ optimiser sitting between the proof and the shipped bytes.

    Instantiating instead leaves both modules untouched.  The adapter's index
    spaces are then fixed by its own import order — [imports_first] below is
    that statement — and the mechanism doing the resolution is the component
    model's, which is the thing this repository formalises anyway.

    Spec: {{https://github.com/WebAssembly/component-model/blob/9a183e56f5c6cc3217895adf54cc4f62de4fa5c9/design/mvp/Explainer.md#core-instance-definitions}} *)

From mathcomp Require Import ssreflect ssrbool eqtype seq.

From Wasm Require Import list_extra datatypes datatypes_properties operations opsem host
  instantiation_spec properties.

From Stdlib Require Import BinNat List Lia
  Relations.Relations Relations.Relation_Operators.

From wasmcomponents Require Import component_ast.

Import ListNotations.

(** ** Resolving two-level imports

    Core Wasm imports carry two names.  A component supplies one core instance
    per *first-level* name, so the adapter's [(import "rocq" "memory" ...)] is
    resolved by finding ["rocq"] among the instantiation arguments and then
    ["memory"] among that instance's exports.  No renaming, no renumbering:
    the result is the backend's own address. *)

(** The core instance index space, in definition order. *)
Definition core_instances := list moduleinst.

Definition lookup_export (mi : moduleinst) (n : name) : option extern_value :=
  option_map exportinst_val
    (List.find (fun e => e.(exportinst_name) == n) mi.(inst_exports)).

Definition resolve_import (cis : core_instances)
                          (args : list core_instantiate_arg)
                          (imp : module_import) : option extern_value :=
  match List.find (fun a => a.(cia_name) == imp.(imp_module)) args with
  | Some a =>
      match lookup_N cis a.(cia_instance) with
      | Some mi => lookup_export mi imp.(imp_name)
      | None => None
      end
  | None => None
  end.

(** The import vector [instantiate] wants, in the module's own import order —
    which is the whole point: that order is in the module's text. *)
Definition resolve_imports (cis : core_instances)
                           (args : list core_instantiate_arg)
                           (m : module) : option (list extern_value) :=
  those (List.map (resolve_import cis args) m.(mod_imports)).

Lemma resolve_import_spec : forall cis args imp ev,
  resolve_import cis args imp = Some ev ->
  exists a mi,
    List.In a args /\
    a.(cia_name) = imp.(imp_module) /\
    lookup_N cis a.(cia_instance) = Some mi /\
    lookup_export mi imp.(imp_name) = Some ev.
Proof.
  intros cis args imp ev H. unfold resolve_import in H.
  destruct (List.find (fun a => a.(cia_name) == imp.(imp_module)) args) as [a|] eqn:Hf;
    [| discriminate].
  destruct (lookup_N cis a.(cia_instance)) as [mi|] eqn:Hi; [| discriminate].
  apply List.find_some in Hf as [Hin Heq].
  exists a, mi. repeat split; auto. by apply/eqP.
Qed.

(** [those] is tail-recursive; [those_those0] turns it into the plain fixpoint
    these two go by induction on. *)
Lemma those0_map_nth : forall A B (g : A -> option B) xs ys i x,
  those0 (List.map g xs) = Some ys ->
  List.nth_error xs i = Some x ->
  List.nth_error ys i = g x.
Proof.
  intros A B g xs. induction xs as [| a xs IH]; intros ys i x Hth Hnth.
  - destruct i; discriminate.
  - cbn in Hth. destruct (g a) as [b|] eqn:Hga; [| discriminate].
    destruct (those0 (List.map g xs)) as [bs|] eqn:Hbs; [| discriminate].
    cbn in Hth. injection Hth as <-. destruct i as [| i].
    + cbn in Hnth. injection Hnth as <-. cbn. by rewrite Hga.
    + cbn in Hnth |- *. by apply IH.
Qed.

Lemma those0_map_length : forall A B (g : A -> option B) xs ys,
  those0 (List.map g xs) = Some ys ->
  List.length ys = List.length xs.
Proof.
  intros A B g xs. induction xs as [| a xs IH]; intros ys Hth.
  - cbn in Hth. by injection Hth as <-.
  - cbn in Hth. destruct (g a) as [b|]; [| discriminate].
    destruct (those0 (List.map g xs)) as [bs|] eqn:Hbs; [| discriminate].
    cbn in Hth. injection Hth as <-. cbn. f_equal. by apply IH.
Qed.

(** Resolution preserves position, so an import's index in the module is its
    index in the vector handed to [instantiate].  This is what lets the
    adapter's proof talk about "global 0" and mean "the first global this
    module imports". *)
Lemma resolve_imports_nth : forall cis args m v_imps i imp,
  resolve_imports cis args m = Some v_imps ->
  List.nth_error m.(mod_imports) i = Some imp ->
  List.nth_error v_imps i = resolve_import cis args imp.
Proof.
  intros cis args m v_imps i imp Hres Hnth. unfold resolve_imports in Hres.
  rewrite -those_those0 in Hres. by eapply those0_map_nth; eauto.
Qed.

Lemma resolve_imports_length : forall cis args m v_imps,
  resolve_imports cis args m = Some v_imps ->
  List.length v_imps = List.length m.(mod_imports).
Proof.
  intros cis args m v_imps Hres. unfold resolve_imports in Hres.
  rewrite -those_those0 in Hres. by eapply those0_map_length; eauto.
Qed.

Section CoreInstantiate.

Context `{ho : host}.

(** ** Running one definition

    [instantiate] allocates and returns the element- and data-segment
    initialisers together with the start function, as instructions to execute
    in the new frame.  A component's core instantiation runs them before the
    next instance is created, which is why the backend's data segments and
    start are in place by the time the adapter is instantiated against it. *)
Definition run_init (hs : host_state) (s : store_record) (f : frame)
                    (bes : list basic_instruction)
                    (hs' : host_state) (s' : store_record) : Prop :=
  reduce_trans (hs, s, f, to_e_list bes) (hs', s', f, [::]).

(** The component's core index spaces for the *other* sorts, which only
    aliases populate.  Passed in as a lookup because this file models the
    core instance section, not the alias section; a component with no core
    aliases (such as the ones here) never consults it. *)
Definition core_env := core_sort -> N -> option extern_value.

Inductive core_instance_step (mods : list module) (cenv : core_env)
  : host_state -> store_record -> core_instances -> core_instance_def ->
    host_state -> store_record -> moduleinst -> Prop :=
| cis_instantiate : forall hs s cis midx args m v_imps s1 f bes hs2 s2,
    lookup_N mods midx = Some m ->
    resolve_imports cis args m = Some v_imps ->
    instantiate s m v_imps (s1, f, bes) ->
    run_init hs s1 f bes hs2 s2 ->
    core_instance_step mods cenv hs s cis
      (CID_instantiate midx args) hs2 s2 f.(f_inst)
| cis_inline : forall hs s cis exps evs,
    those (List.map (fun e => cenv e.(cie_sort) e.(cie_idx)) exps) = Some evs ->
    core_instance_step mods cenv hs s cis
      (CID_inline_exports exps) hs s
      (Build_moduleinst [::] [::] [::] [::] [::] [::] [::]
         (List.map (fun ne => Build_exportinst (fst ne) (snd ne))
                   (List.combine (List.map cie_name exps) evs))).

(** The section, left to right: each definition sees the instances before it,
    and appends its own. *)
Inductive core_instantiate (mods : list module) (cenv : core_env)
  : host_state -> store_record -> core_instances -> list core_instance_def ->
    host_state -> store_record -> core_instances -> Prop :=
| ci_nil : forall hs s cis,
    core_instantiate mods cenv hs s cis [::] hs s cis
| ci_cons : forall hs s cis d ds hs1 s1 mi hs2 s2 cis2,
    core_instance_step mods cenv hs s cis d hs1 s1 mi ->
    core_instantiate mods cenv hs1 s1 (cis ++ [:: mi]) ds hs2 s2 cis2 ->
    core_instantiate mods cenv hs s cis (d :: ds) hs2 s2 cis2.

(** ** Imports come first

    [alloc_module] puts the imported addresses at the bottom of every index
    space ([instantiation_spec.v:266-269]).  So a module's own definitions are
    numbered after what it imports, and a module that defines none of a sort
    indexes that sort *exactly* by its own import order.

    For the adapter, whose imports are, in order,
    [memory], [mem_ptr], [result], [out_of_mem], [main_function], [table]:
    memory 0 and table 0 are the backend's; globals 0, 1, 2 are [mem_ptr],
    [result], [out_of_mem]; function 0 is [main_function], and the adapter's
    own three bodies follow at 1, 2 and 3.

    Compare what the merged module did with the same two halves: binaryen kept
    the functions where they were and permuted the globals. *)

(** Everything below goes through [alloc_module] rather than [instantiate],
    because that is where the index spaces are built; [instantiate] only
    carries it.  Note [seq.map], not [List.map]: [instantiation_spec.v] never
    imports [Stdlib List], so the [let] scrutinees there are ssreflect's
    [map], and a [destruct] naming the other one splits an unrelated goal and
    leaves the [let] standing. *)
Lemma alloc_module_shape : forall s m imps gvs rvs s' inst,
  alloc_module s m imps gvs rvs (s', inst) ->
  exists i_fs i_ts i_ms i_gs,
    inst.(inst_types)   = m.(mod_types) /\
    inst.(inst_funcs)   = ext_funcs   imps ++ i_fs /\
    inst.(inst_tables)  = ext_tables  imps ++ i_ts /\
    inst.(inst_mems)    = ext_mems    imps ++ i_ms /\
    inst.(inst_globals) = ext_globals imps ++ i_gs.
Proof.
  intros s m imps gvs rvs s' inst H.
  unfold alloc_module in H.
  destruct (alloc_funcs s m.(mod_funcs) inst) as [s1 i_fs].
  destruct (alloc_tabs s1 (seq.map modtab_type m.(mod_tables))) as [s2 i_ts].
  destruct (alloc_mems s2 (seq.map modmem_type m.(mod_mems))) as [s3 i_ms].
  destruct (alloc_globs s3 m.(mod_globals) gvs) as [s4 i_gs].
  destruct (alloc_elems s4 m.(mod_elems) rvs) as [s5 i_es].
  destruct (alloc_datas s5 m.(mod_datas)) as [s6 i_ds].
  destruct H as [_ [Hty [Hfu [Hta [Hme [Hgl _]]]]]].
  by exists i_fs, i_ts, i_ms, i_gs.
Qed.

(** Allocating nothing adds nothing, which is what turns the shape above into
    an equation for a module that only imports.  [alloc_globs] takes the
    initial values alongside the definitions and zips them, so a module with
    no globals reduces whatever [gvs] the instantiation chose. *)
Lemma alloc_module_globals_imported : forall s m imps gvs rvs s' inst,
  alloc_module s m imps gvs rvs (s', inst) ->
  m.(mod_globals) = [::] ->
  inst.(inst_globals) = ext_globals imps.
Proof.
  intros s m imps gvs rvs s' inst H Hng.
  unfold alloc_module in H. rewrite Hng in H.
  destruct (alloc_funcs s m.(mod_funcs) inst) as [s1 i_fs].
  destruct (alloc_tabs s1 (seq.map modtab_type m.(mod_tables))) as [s2 i_ts].
  destruct (alloc_mems s2 (seq.map modmem_type m.(mod_mems))) as [s3 i_ms].
  cbn in H.
  destruct (alloc_elems s3 m.(mod_elems) rvs) as [s5 i_es].
  destruct (alloc_datas s5 m.(mod_datas)) as [s6 i_ds].
  destruct H as [_ [_ [_ [_ [_ [Hgl _]]]]]].
  rewrite Hgl. by rewrite cats0.
Qed.

Lemma alloc_module_mems_imported : forall s m imps gvs rvs s' inst,
  alloc_module s m imps gvs rvs (s', inst) ->
  m.(mod_mems) = [::] ->
  inst.(inst_mems) = ext_mems imps.
Proof.
  intros s m imps gvs rvs s' inst H Hnm.
  unfold alloc_module in H. rewrite Hnm in H.
  destruct (alloc_funcs s m.(mod_funcs) inst) as [s1 i_fs].
  destruct (alloc_tabs s1 (seq.map modtab_type m.(mod_tables))) as [s2 i_ts].
  cbn in H.
  destruct (alloc_globs s2 m.(mod_globals) gvs) as [s4 i_gs].
  destruct (alloc_elems s4 m.(mod_elems) rvs) as [s5 i_es].
  destruct (alloc_datas s5 m.(mod_datas)) as [s6 i_ds].
  destruct H as [_ [_ [_ [_ [Hme _]]]]].
  rewrite Hme. by rewrite cats0.
Qed.

Lemma alloc_module_tables_imported : forall s m imps gvs rvs s' inst,
  alloc_module s m imps gvs rvs (s', inst) ->
  m.(mod_tables) = [::] ->
  inst.(inst_tables) = ext_tables imps.
Proof.
  intros s m imps gvs rvs s' inst H Hnt.
  unfold alloc_module in H. rewrite Hnt in H.
  destruct (alloc_funcs s m.(mod_funcs) inst) as [s1 i_fs].
  cbn in H.
  destruct (alloc_mems s1 (seq.map modmem_type m.(mod_mems))) as [s3 i_ms].
  destruct (alloc_globs s3 m.(mod_globals) gvs) as [s4 i_gs].
  destruct (alloc_elems s4 m.(mod_elems) rvs) as [s5 i_es].
  destruct (alloc_datas s5 m.(mod_datas)) as [s6 i_ds].
  destruct H as [_ [_ [_ [Hta _]]]].
  rewrite Hta. by rewrite cats0.
Qed.


(** [instantiate] hands the allocation out; these four just unwrap it.  The
    [frame] it yields is the one [core_instance_step] records as the new core
    instance, so these are statements about that instance's index spaces. *)
Lemma instantiate_alloc : forall s m v_imps s' f bes,
  instantiate s m v_imps (s', f, bes) ->
  exists g_inits r_inits, alloc_module s m v_imps g_inits r_inits (s', f.(f_inst)).
Proof.
  intros s m v_imps s' f bes Hinst.
  unfold instantiate in Hinst. cbv beta iota in Hinst.
  destruct Hinst as [t_imps_mod [t_imps [t_exps [hs' [inst [g_inits [r_inits
    [Htyping [Hext [Hsub [Halloc [Hg [He [Hf Hbes]]]]]]]]]]]]]].
  exists g_inits, r_inits. subst f. by cbn.
Qed.

Lemma instantiate_inst_shape : forall s m v_imps s' f bes,
  instantiate s m v_imps (s', f, bes) ->
  exists i_fs i_ts i_ms i_gs,
    (f.(f_inst)).(inst_types)   = m.(mod_types) /\
    (f.(f_inst)).(inst_funcs)   = ext_funcs   v_imps ++ i_fs /\
    (f.(f_inst)).(inst_tables)  = ext_tables  v_imps ++ i_ts /\
    (f.(f_inst)).(inst_mems)    = ext_mems    v_imps ++ i_ms /\
    (f.(f_inst)).(inst_globals) = ext_globals v_imps ++ i_gs.
Proof.
  intros s m v_imps s' f bes Hinst.
  apply instantiate_alloc in Hinst as [g_inits [r_inits Halloc]].
  by eapply alloc_module_shape; eauto.
Qed.

(** A module that defines no globals indexes its globals by its import order
    alone.  The adapter is such a module, and so are the two below it. *)
Lemma instantiate_globals_imported : forall s m v_imps s' f bes,
  instantiate s m v_imps (s', f, bes) ->
  m.(mod_globals) = [::] ->
  (f.(f_inst)).(inst_globals) = ext_globals v_imps.
Proof.
  intros s m v_imps s' f bes Hinst Hng.
  apply instantiate_alloc in Hinst as [g_inits [r_inits Halloc]].
  by eapply alloc_module_globals_imported; eauto.
Qed.

Lemma instantiate_mems_imported : forall s m v_imps s' f bes,
  instantiate s m v_imps (s', f, bes) ->
  m.(mod_mems) = [::] ->
  (f.(f_inst)).(inst_mems) = ext_mems v_imps.
Proof.
  intros s m v_imps s' f bes Hinst Hnm.
  apply instantiate_alloc in Hinst as [g_inits [r_inits Halloc]].
  by eapply alloc_module_mems_imported; eauto.
Qed.

Lemma instantiate_tables_imported : forall s m v_imps s' f bes,
  instantiate s m v_imps (s', f, bes) ->
  m.(mod_tables) = [::] ->
  (f.(f_inst)).(inst_tables) = ext_tables v_imps.
Proof.
  intros s m v_imps s' f bes Hinst Hnt.
  apply instantiate_alloc in Hinst as [g_inits [r_inits Halloc]].
  by eapply alloc_module_tables_imported; eauto.
Qed.

End CoreInstantiate.
