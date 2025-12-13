"""
Inferno Cognitive Kernel Package

Provides Python interface to the Inferno cognitive kernel for AGI development.
"""

__version__ = "1.0.0"

# Import main components for easy access
import sys
from pathlib import Path

# Add lib directory to path
lib_path = Path(__file__).parent.parent / "inferno-kernel" / "lib"
sys.path.insert(0, str(lib_path))

try:
    from inferno_bridge import (
        InfernoKernelBridge,
        TruthValue,
        Atom,
        Link,
        Inference
    )
    
    __all__ = [
        'InfernoKernelBridge',
        'TruthValue',
        'Atom',
        'Link',
        'Inference'
    ]
except ImportError:
    # If not available, provide a helpful message
    import warnings
    warnings.warn(
        "Inferno kernel bridge not available. "
        "Ensure inferno_bridge.py is in opencog/inferno-kernel/lib/"
    )
    __all__ = []
