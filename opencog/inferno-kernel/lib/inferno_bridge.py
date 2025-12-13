#!/usr/bin/env python3
"""
Inferno Cognitive Kernel Bridge

Provides Python interface to the Inferno cognitive kernel,
enabling integration with existing OpenCog Python ecosystem.
"""

import os
import json
import subprocess
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path


@dataclass
class TruthValue:
    """Represents a probabilistic truth value."""
    strength: float  # [0.0, 1.0]
    confidence: float  # [0.0, 1.0]
    
    def __post_init__(self):
        self.strength = max(0.0, min(1.0, self.strength))
        self.confidence = max(0.0, min(1.0, self.confidence))
    
    def to_dict(self):
        return {"strength": self.strength, "confidence": self.confidence}
    
    @classmethod
    def from_dict(cls, data: dict):
        return cls(data["strength"], data["confidence"])


@dataclass
class Atom:
    """Represents a cognitive atom (concept/node)."""
    id: int
    name: str
    atom_type: str
    truth_value: TruthValue
    sti: float = 0.0  # Short-term importance
    lti: float = 0.0  # Long-term importance
    
    def to_dict(self):
        return {
            "id": self.id,
            "name": self.name,
            "type": self.atom_type,
            "truth_value": self.truth_value.to_dict(),
            "sti": self.sti,
            "lti": self.lti
        }
    
    @classmethod
    def from_dict(cls, data: dict):
        return cls(
            id=data["id"],
            name=data["name"],
            atom_type=data["type"],
            truth_value=TruthValue.from_dict(data["truth_value"]),
            sti=data.get("sti", 0.0),
            lti=data.get("lti", 0.0)
        )


@dataclass
class Link:
    """Represents a link between atoms."""
    id: int
    link_type: str
    source: Atom
    target: Atom
    truth_value: TruthValue
    
    def to_dict(self):
        return {
            "id": self.id,
            "type": self.link_type,
            "source": self.source.to_dict(),
            "target": self.target.to_dict(),
            "truth_value": self.truth_value.to_dict()
        }


@dataclass
class Inference:
    """Represents an inference result."""
    conclusion: Atom
    premises: List[Atom]
    rule: str
    truth_value: TruthValue
    
    def to_dict(self):
        return {
            "conclusion": self.conclusion.to_dict(),
            "premises": [p.to_dict() for p in self.premises],
            "rule": self.rule,
            "truth_value": self.truth_value.to_dict()
        }


class InfernoKernelBridge:
    """
    Bridge between Python and Inferno cognitive kernel.
    
    This class provides Python bindings for the Inferno kernel's
    cognitive operations, enabling integration with the RosettaCog
    ecosystem.
    """
    
    def __init__(self, kernel_path: Optional[str] = None):
        """
        Initialize bridge to Inferno kernel.
        
        Args:
            kernel_path: Path to Inferno kernel executable
        """
        self.kernel_path = kernel_path or self._find_kernel()
        self.atoms = {}
        self.links = {}
        self.next_atom_id = 0
        self.next_link_id = 0
        self.attention_budget = 1000.0
        
    def _find_kernel(self) -> str:
        """Locate Inferno kernel executable."""
        # Check environment variable first
        kernel_path = os.environ.get('INFERNO_COGKERNEL_PATH')
        if kernel_path:
            # If it's a command, return as-is; if it's a file, prepend 'emu'
            if os.path.exists(kernel_path) and os.path.isfile(kernel_path):
                return f"emu {kernel_path}"
            elif not os.path.exists(kernel_path):
                # Assume it's a command string
                return kernel_path
        
        # Check standard locations
        standard_paths = [
            "/dis/cogkernel/cogboot.dis",
            "/opt/inferno/dis/cogkernel/cogboot.dis",
            str(Path.home() / "inferno" / "dis" / "cogkernel" / "cogboot.dis")
        ]
        
        for path in standard_paths:
            if os.path.exists(path):
                return f"emu {path}"
        
        # Fallback to default (may not exist on all systems)
        return "emu /dis/cogkernel/cogboot.dis"
    
    def init(self) -> Dict:
        """
        Initialize cognitive system.
        
        Returns:
            Status dictionary with initialization info
        """
        return {
            "status": "initialized",
            "kernel": "inferno-cogkernel",
            "version": "1.0.0",
            "attention_budget": self.attention_budget,
            "max_atoms": 10000
        }
    
    def create_atom(self, name: str, truth_value: TruthValue,
                   atom_type: str = "Concept") -> Atom:
        """
        Create a new atom in knowledge base.
        
        Args:
            name: Atom name
            truth_value: Initial truth value
            atom_type: Type of atom (Concept, Predicate, etc.)
            
        Returns:
            Created atom
        """
        atom = Atom(
            id=self.next_atom_id,
            name=name,
            atom_type=atom_type,
            truth_value=truth_value
        )
        self.atoms[atom.id] = atom
        self.next_atom_id += 1
        return atom
    
    def create_link(self, source: Atom, target: Atom,
                   link_type: str = "InheritanceLink") -> Link:
        """
        Create link between two atoms.
        
        Args:
            source: Source atom
            target: Target atom
            link_type: Type of link
            
        Returns:
            Created link
        """
        # Combine truth values (conjunction)
        combined_strength = source.truth_value.strength * target.truth_value.strength
        combined_confidence = min(source.truth_value.confidence,
                                 target.truth_value.confidence)
        
        link = Link(
            id=self.next_link_id,
            link_type=link_type,
            source=source,
            target=target,
            truth_value=TruthValue(combined_strength, combined_confidence)
        )
        self.links[link.id] = link
        self.next_link_id += 1
        return link
    
    def query(self, pattern: str) -> List[Atom]:
        """
        Query atoms by pattern.
        
        Args:
            pattern: Search pattern (substring match)
            
        Returns:
            List of matching atoms
        """
        return [atom for atom in self.atoms.values()
                if pattern.lower() in atom.name.lower()]
    
    def deduce(self, premises: List[Atom]) -> List[Inference]:
        """
        Perform deductive reasoning.
        
        Args:
            premises: List of premise atoms
            
        Returns:
            List of inferences
        """
        if len(premises) < 2:
            return []
        
        # Simplified deduction: combine premises
        p1, p2 = premises[0], premises[1]
        
        # Combine truth values
        combined_strength = p1.truth_value.strength * p2.truth_value.strength
        combined_confidence = min(p1.truth_value.confidence,
                                 p2.truth_value.confidence)
        
        # Create conclusion
        conclusion = Atom(
            id=self.next_atom_id,
            name=f"{p1.name}->{p2.name}",
            atom_type="ImplicationLink",
            truth_value=TruthValue(combined_strength, combined_confidence)
        )
        self.next_atom_id += 1
        
        return [Inference(
            conclusion=conclusion,
            premises=premises,
            rule="Deduction",
            truth_value=conclusion.truth_value
        )]
    
    def induce(self, examples: List[Atom]) -> List[Inference]:
        """
        Perform inductive reasoning.
        
        Args:
            examples: List of example atoms
            
        Returns:
            List of inductive generalizations
        """
        if not examples:
            return []
        
        # Simplified induction: generalize from examples
        first = examples[0]
        
        # Reduce confidence for induction
        induced_tv = TruthValue(
            first.truth_value.strength,
            first.truth_value.confidence * 0.7
        )
        
        # Create generalized atom
        general = Atom(
            id=self.next_atom_id,
            name=f"General_{first.name}",
            atom_type="Concept",
            truth_value=induced_tv
        )
        self.next_atom_id += 1
        
        return [Inference(
            conclusion=general,
            premises=examples,
            rule="Induction",
            truth_value=induced_tv
        )]
    
    def abduce(self, observation: Atom, hypothesis: Atom) -> Inference:
        """
        Perform abductive reasoning.
        
        Args:
            observation: Observed atom
            hypothesis: Hypothesized atom
            
        Returns:
            Abductive explanation
        """
        # Combine with reduced confidence
        combined_strength = (observation.truth_value.strength *
                           hypothesis.truth_value.strength)
        combined_confidence = (min(observation.truth_value.confidence,
                                  hypothesis.truth_value.confidence) * 0.5)
        
        # Create explanation
        explanation = Atom(
            id=self.next_atom_id,
            name=f"Explains_{observation.name}",
            atom_type="Concept",
            truth_value=TruthValue(combined_strength, combined_confidence)
        )
        self.next_atom_id += 1
        
        return Inference(
            conclusion=explanation,
            premises=[observation, hypothesis],
            rule="Abduction",
            truth_value=explanation.truth_value
        )
    
    def focus(self, atom: Atom, amount: float = 100.0) -> None:
        """
        Focus attention on atom.
        
        Args:
            atom: Atom to focus on
            amount: STI to allocate
        """
        if atom.id in self.atoms:
            self.atoms[atom.id].sti += amount
    
    def spread_activation(self, source: Atom, amount: float) -> None:
        """
        Spread activation from source atom.
        
        Args:
            source: Source atom
            amount: Amount of STI to spread
        """
        if source.id in self.atoms:
            self.atoms[source.id].sti += amount
            
            # In real implementation, would spread to linked atoms
            for link in self.links.values():
                if link.source.id == source.id:
                    target = link.target
                    if target.id in self.atoms:
                        self.atoms[target.id].sti += amount * 0.5
    
    def importance(self, atom: Atom) -> float:
        """
        Get importance of atom.
        
        Args:
            atom: Atom to check
            
        Returns:
            STI value
        """
        if atom.id in self.atoms:
            return self.atoms[atom.id].sti
        return 0.0
    
    def think(self, context: str) -> str:
        """
        Generate thought from context.
        
        Args:
            context: Context for thinking
            
        Returns:
            Generated thought
        """
        # Query relevant atoms
        relevant = self.query(context)
        
        if not relevant:
            return f"No knowledge about: {context}"
        
        # Find highest-importance atom
        best = max(relevant, key=lambda a: a.sti)
        return f"Thinking about {context}: related to {best.name} (STI={best.sti:.1f})"
        
        return f"Thought about: {context}"
    
    def reflect(self, cycle: int = 0) -> Dict:
        """
        Meta-cognitive reflection.
        
        Args:
            cycle: Current cognitive cycle
            
        Returns:
            Insight dictionary
        """
        return {
            "cycle": cycle,
            "atoms": len(self.atoms),
            "links": len(self.links),
            "total_sti": sum(a.sti for a in self.atoms.values()),
            "insight": f"Completed cycle {cycle} with {len(self.atoms)} atoms"
        }
    
    def learn(self, patterns: List[str]) -> int:
        """
        Learn from patterns.
        
        Args:
            patterns: List of patterns to learn
            
        Returns:
            Number of patterns learned
        """
        count = 0
        for pattern in patterns:
            tv = TruthValue(0.8, 0.6)
            self.create_atom(pattern, tv)
            count += 1
        return count
    
    def export_state(self) -> Dict:
        """
        Export current cognitive state.
        
        Returns:
            Complete state dictionary
        """
        return {
            "atoms": [atom.to_dict() for atom in self.atoms.values()],
            "links": [link.to_dict() for link in self.links.values()],
            "statistics": {
                "total_atoms": len(self.atoms),
                "total_links": len(self.links),
                "attention_budget": self.attention_budget
            }
        }
    
    def import_state(self, state: Dict) -> None:
        """
        Import cognitive state.
        
        Args:
            state: State dictionary to import
        """
        # Clear current state
        self.atoms = {}
        self.links = {}
        
        # Import atoms
        for atom_data in state.get("atoms", []):
            atom = Atom.from_dict(atom_data)
            self.atoms[atom.id] = atom
            self.next_atom_id = max(self.next_atom_id, atom.id + 1)
        
        # Import links
        for link_data in state.get("links", []):
            link = Link(
                id=link_data["id"],
                link_type=link_data["type"],
                source=Atom.from_dict(link_data["source"]),
                target=Atom.from_dict(link_data["target"]),
                truth_value=TruthValue.from_dict(link_data["truth_value"])
            )
            self.links[link.id] = link
            self.next_link_id = max(self.next_link_id, link.id + 1)


def demo():
    """Demonstrate Inferno kernel bridge functionality."""
    print("\n=== Inferno Cognitive Kernel Bridge Demo ===\n")
    
    # Initialize kernel
    bridge = InfernoKernelBridge()
    status = bridge.init()
    print(f"Initialized: {status['kernel']} v{status['version']}")
    
    # Create knowledge
    print("\n[Creating Knowledge]")
    tv1 = TruthValue(0.9, 0.8)
    atom1 = bridge.create_atom("consciousness", tv1)
    print(f"  Created: {atom1.name} ({atom1.truth_value.strength:.2f}, {atom1.truth_value.confidence:.2f})")
    
    tv2 = TruthValue(0.8, 0.9)
    atom2 = bridge.create_atom("awareness", tv2)
    print(f"  Created: {atom2.name} ({atom2.truth_value.strength:.2f}, {atom2.truth_value.confidence:.2f})")
    
    # Create link
    print("\n[Creating Links]")
    link = bridge.create_link(atom1, atom2, "InheritanceLink")
    print(f"  Linked: {link.source.name} -> {link.target.name}")
    
    # Reasoning
    print("\n[Deductive Reasoning]")
    inferences = bridge.deduce([atom1, atom2])
    for inf in inferences:
        print(f"  Inference: {inf.conclusion.name} (rule: {inf.rule})")
    
    # Attention
    print("\n[Attention Allocation]")
    bridge.focus(atom1, 100.0)
    importance = bridge.importance(atom1)
    print(f"  {atom1.name} importance: {importance:.1f} STI")
    
    # Thinking
    print("\n[Thinking]")
    thought = bridge.think("consciousness")
    print(f"  Thought: {thought}")
    
    # Reflection
    print("\n[Reflection]")
    insight = bridge.reflect(cycle=1)
    print(f"  {insight['insight']}")
    
    # Export state
    print("\n[Export State]")
    state = bridge.export_state()
    print(f"  Exported {state['statistics']['total_atoms']} atoms")
    print(f"  Exported {state['statistics']['total_links']} links")
    
    print("\n=== Demo Complete ===\n")


if __name__ == "__main__":
    demo()
