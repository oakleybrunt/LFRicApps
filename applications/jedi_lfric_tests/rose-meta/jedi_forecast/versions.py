import re
import sys
from metomi.rose.upgrade import MacroUpgrade


class UpgradeError(Exception):
    """Exception created when an upgrade fails."""

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        sys.tracebacklimit = 0
        return self.msg

    __str__ = __repr__


"""
Copy this template and complete to add your macro

class vnXX_txxx(MacroUpgrade):
    # Upgrade macro for <TICKET> by <Author>

    BEFORE_TAG = "vnX.X"
    AFTER_TAG = "vnX.X_txxx"

    def upgrade(self, config, meta_config=None):
        # Add settings
        return config, self.reports
"""


class vn20_t85(MacroUpgrade):
    """Upgrade macro for ticket #85 by Chris Smith."""

    BEFORE_TAG = "vn2.0"
    AFTER_TAG = "vn2.0_t85"

    def upgrade(self, config, meta_config=None):
        # Commands From: science/gungho/rose-meta/lfric-gungho
        """Add new geostrophic_forcing namelist"""
        source = self.get_setting_value(
            config, ["file:configuration.nml", "source"]
        )
        source = re.sub(
            r"namelist:formulation",
            r"namelist:formulation" + "\n" + " (namelist:geostrophic_forcing)",
            source,
        )
        self.change_setting_value(
            config, ["file:configuration.nml", "source"], source
        )
        """Add geostrophic_forcing setting to external_forcing namelist"""
        self.add_setting(
            config,
            ["namelist:external_forcing", "geostrophic_forcing"],
            ".false.",
        )
        """Add default data for geostrophic_forcing namelist"""
        self.add_setting(config, ["namelist:geostrophic_forcing"])
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "coordinate"], "'height'"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "heights"], "0.0"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "number_heights"], "1"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "number_times"], "1"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "profile_data_u"], "0.0"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "profile_data_v"], "0.0"
        )
        self.add_setting(
            config, ["namelist:geostrophic_forcing", "times"], "0.0"
        )

        return config, self.reports
