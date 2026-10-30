"""Import a script in this directory as a module, for testing."""

import importlib.machinery
import importlib.util
import sys
import types
from pathlib import Path

SCRIPT_DIRECTORY = Path(__file__).parent


def load_script(file_name: str) -> types.ModuleType:
    """Import a script in this directory, whose name need not be a legal module name.

    The module's name is the script's name, without its extension and with each "-" replaced by
    "_".

    Args:
        file_name: the script's file name, such as "pr-status.py"

    Returns:
        The imported module.
    """
    module_name = Path(file_name).stem.replace("-", "_")
    loader = importlib.machinery.SourceFileLoader(module_name, str(SCRIPT_DIRECTORY / file_name))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module
