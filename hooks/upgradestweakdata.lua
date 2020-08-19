local original_player_definitions = UpgradesTweakData._player_definitions

function UpgradesTweakData:_player_definitions(...)
	original_player_definitions(self, ...)
	
	
	self.values.player.chico_injector_health_to_speed = {
		{
			0.5,
			1
		}
	}
end