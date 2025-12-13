# Cognitive Kernel Interface
# Core cognitive operating system primitives

CogKernel: module {
    PATH: con "/dis/cogkernel.dis";
    
    # Core cognitive data structures
    
    CogSystem: adt {
        atomspace: ref AtomSpace;
        pln: ref PLN;
        ecan: ref ECAN;
        cogloop: ref CogLoop;
        config: ref Config;
    };
    
    AtomSpace: adt {
        atoms: list of ref Atom;
        links: list of ref Link;
        index: ref Index;
        
        create: fn(a: self ref AtomSpace, name: string, tv: ref TruthValue): ref Atom;
        link: fn(a: self ref AtomSpace, source: ref Atom, target: ref Atom, 
                 linktype: string): ref Link;
        query: fn(a: self ref AtomSpace, pattern: string): list of ref Atom;
        gettruth: fn(a: self ref AtomSpace, atom: ref Atom): ref TruthValue;
    };
    
    Atom: adt {
        id: int;
        name: string;
        atomtype: string;
        tv: ref TruthValue;
        sti: real;  # Short-term importance
        lti: real;  # Long-term importance
        av: ref AttentionValue;
    };
    
    Link: adt {
        id: int;
        linktype: string;
        source: ref Atom;
        target: ref Atom;
        tv: ref TruthValue;
    };
    
    TruthValue: adt {
        strength: real;    # Probability [0,1]
        confidence: real;  # Confidence [0,1]
        
        new: fn(s: real, c: real): ref TruthValue;
        revise: fn(tv1: self ref TruthValue, tv2: ref TruthValue): ref TruthValue;
        conjunction: fn(tv1: self ref TruthValue, tv2: ref TruthValue): ref TruthValue;
    };
    
    AttentionValue: adt {
        sti: real;  # Short-term importance (unbounded, typically -1000 to 1000)
        lti: real;  # Long-term importance [0,1]
        vlti: int;  # Very long-term importance flag
    };
    
    # Probabilistic Logic Networks
    
    PLN: adt {
        deduce: fn(pln: self ref PLN, premises: list of ref Atom): list of ref Inference;
        induce: fn(pln: self ref PLN, examples: list of ref Atom): list of ref Inference;
        abduce: fn(pln: self ref PLN, observation: ref Atom, 
                   hypothesis: ref Atom): ref Inference;
        unify: fn(pln: self ref PLN, pattern: ref Atom, 
                  target: ref Atom): ref Substitution;
    };
    
    Inference: adt {
        conclusion: ref Atom;
        premises: list of ref Atom;
        rule: string;
        tv: ref TruthValue;
    };
    
    Substitution: adt {
        bindings: list of (string, ref Atom);
        tv: ref TruthValue;
    };
    
    # Economic Attention Networks
    
    ECAN: adt {
        bank: ref AttentionBank;
        
        spread: fn(ecan: self ref ECAN, source: ref Atom, amount: real): int;
        focus: fn(ecan: self ref ECAN, target: ref Atom): int;
        stimulate: fn(ecan: self ref ECAN, atoms: list of ref Atom): int;
        importance: fn(ecan: self ref ECAN, atom: ref Atom): real;
    };
    
    AttentionBank: adt {
        total_sti: real;
        atoms: list of ref Atom;
        
        allocate: fn(bank: self ref AttentionBank, atom: ref Atom, sti: real): int;
        decay: fn(bank: self ref AttentionBank, rate: real): int;
        normalize: fn(bank: self ref AttentionBank): int;
    };
    
    # Cognitive Loop
    
    CogLoop: adt {
        state: string;
        cycle: int;
        
        perceive: fn(loop: self ref CogLoop, stimuli: list of ref Atom): int;
        reason: fn(loop: self ref CogLoop): list of ref Inference;
        act: fn(loop: self ref CogLoop, actions: list of ref Atom): int;
        reflect: fn(loop: self ref CogLoop): ref Insight;
    };
    
    Insight: adt {
        content: string;
        atoms: list of ref Atom;
        tv: ref TruthValue;
    };
    
    # Configuration
    
    Config: adt {
        max_atoms: int;
        attention_budget: real;
        decay_rate: real;
        learning_rate: real;
    };
    
    Index: adt {
        # Fast lookup structures for atoms
        byname: list of (string, ref Atom);
        bytype: list of (string, list of ref Atom);
        byimportance: list of ref Atom;
    };
    
    # Main kernel interface
    
    init: fn(): ref CogSystem;
    shutdown: fn(sys: ref CogSystem): int;
    
    # Cognitive syscalls
    
    sys_think: fn(sys: ref CogSystem, context: string): string;
    sys_reason: fn(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference;
    sys_learn: fn(sys: ref CogSystem, patterns: list of string): int;
    sys_attend: fn(sys: ref CogSystem, focus: ref Atom): int;
    sys_reflect: fn(sys: ref CogSystem): ref Insight;
    
    # AtomSpace syscalls
    
    sys_atom_create: fn(sys: ref CogSystem, name: string, 
                        tv: ref TruthValue): ref Atom;
    sys_atom_link: fn(sys: ref CogSystem, source: ref Atom, 
                      target: ref Atom, linktype: string): ref Link;
    sys_atom_query: fn(sys: ref CogSystem, pattern: string): list of ref Atom;
    
    # PLN syscalls
    
    sys_deduce: fn(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference;
    sys_induce: fn(sys: ref CogSystem, examples: list of ref Atom): list of ref Inference;
    sys_abduce: fn(sys: ref CogSystem, observation: ref Atom, 
                   hypothesis: ref Atom): ref Inference;
    
    # ECAN syscalls
    
    sys_spread_activation: fn(sys: ref CogSystem, source: ref Atom, amount: real): int;
    sys_focus: fn(sys: ref CogSystem, target: ref Atom): int;
    sys_importance: fn(sys: ref CogSystem, atom: ref Atom): real;
    
    # Utility functions
    
    newtruthvalue: fn(strength: real, confidence: real): ref TruthValue;
    createatom: fn(sys: ref CogSystem, name: string, tv: ref TruthValue): ref Atom;
    deduce: fn(sys: ref CogSystem, premises: list of ref Atom): list of ref Inference;
    learn: fn(sys: ref CogSystem, patterns: list of string): int;
    think: fn(sys: ref CogSystem, context: string): string;
};
