package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_supple46pl_0_4.java
 * @procedure $dummy_supple.pl_0/4 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_supple46pl_0_4 extends Predicate {
    static Predicate $dummy_supple46pl_0_4_1 = new PRED_$dummy_supple46pl_0_4_1();
    static Predicate $dummy_supple46pl_0_4_2 = new PRED_$dummy_supple46pl_0_4_2();
    static Predicate $dummy_supple46pl_0_4_sub_1 = new PRED_$dummy_supple46pl_0_4_sub_1();

    public Term arg1, arg2, arg3, arg4;

    public PRED_$dummy_supple46pl_0_4(Term a1, Term a2, Term a3, Term a4, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        this.cont = cont;
    }

    public PRED_$dummy_supple46pl_0_4(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_supple46pl_0_4_1, $dummy_supple46pl_0_4_sub_1);
    }

    public int arity() { return 4; }

    public String toString() {
        return "$dummy_supple.pl_0(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ")";
    }
}

class PRED_$dummy_supple46pl_0_4_sub_1 extends PRED_$dummy_supple46pl_0_4 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_supple46pl_0_4_2);
    }
}

class PRED_$dummy_supple46pl_0_4_1 extends PRED_$dummy_supple46pl_0_4 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        return new PRED_$unify_2(a3, a4, cont);
    }
}

class PRED_$dummy_supple46pl_0_4_2 extends PRED_$dummy_supple46pl_0_4 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        a6 = new ListTerm(a2, s1);
        a5 = new ListTerm(a1, a6);
        a8 = new ListTerm(a2, s1);
        a7 = new ListTerm(a1, a8);
        return new PRED_sort_2(a5, a7, cont);
    }
}

