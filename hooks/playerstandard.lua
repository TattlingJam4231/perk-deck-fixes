--[[
   Save Table to File
   Load Table from File
   v 1.0
   
   Lua 5.2 compatible
   
   Only Saves Tables, Numbers and Strings
   Insides Table References are saved
   Does not save Userdata, Metatables, Functions and indices of these
   ----------------------------------------------------
   table.save( table , filename )
   
   on failure: returns an error msg
   
   ----------------------------------------------------
   table.load( filename or stringtable )
   
   Loads a table that has been saved via the table.save function
   
   on success: returns a previously saved table
   on failure: returns as second argument an error msg
   ----------------------------------------------------
   
   Licensed under the same terms as Lua itself.
]]--
do
   -- declare local variables
   --// exportstring( string )
   --// returns a "Lua" portable version of the string
   local function exportstring( s )
      return string.format("%q", s)
   end

   --// The Save Function
   function table.save(  tbl,filename )
      local charS,charE = "   ","\n"
      local file,err = io.open( filename, "wb" )
      if err then return err end

      -- initiate variables for save procedure
      local tables,lookup = { tbl },{ [tbl] = 1 }
      file:write( "return {"..charE )

      for idx,t in ipairs( tables ) do
         file:write( "-- Table: {"..idx.."}"..charE )
         file:write( "{"..charE )
         local thandled = {}

         for i,v in ipairs( t ) do
            thandled[i] = true
            local stype = type( v )
            -- only handle value
            if stype == "table" then
               if not lookup[v] then
                  table.insert( tables, v )
                  lookup[v] = #tables
               end
               file:write( charS.."{"..lookup[v].."},"..charE )
            elseif stype == "string" then
               file:write(  charS..exportstring( v )..","..charE )
            elseif stype == "number" then
               file:write(  charS..tostring( v )..","..charE )
            end
         end

         for i,v in pairs( t ) do
            -- escape handled values
            if (not thandled[i]) then
            
               local str = ""
               local stype = type( i )
               -- handle index
               if stype == "table" then
                  if not lookup[i] then
                     table.insert( tables,i )
                     lookup[i] = #tables
                  end
                  str = charS.."[{"..lookup[i].."}]="
               elseif stype == "string" then
                  str = charS.."["..exportstring( i ).."]="
               elseif stype == "number" then
                  str = charS.."["..tostring( i ).."]="
               end
            
               if str ~= "" then
                  stype = type( v )
                  -- handle value
                  if stype == "table" then
                     if not lookup[v] then
                        table.insert( tables,v )
                        lookup[v] = #tables
                     end
                     file:write( str.."{"..lookup[v].."},"..charE )
                  elseif stype == "string" then
                     file:write( str..exportstring( v )..","..charE )
                  elseif stype == "number" then
                     file:write( str..tostring( v )..","..charE )
                  end
               end
            end
         end
         file:write( "},"..charE )
      end
      file:write( "}" )
      file:close()
   end
   
   --// The Load Function
   function table.load( sfile )
      local ftables,err = loadfile( sfile )
      if err then return _,err end
      local tables = ftables()
      for idx = 1,#tables do
         local tolinki = {}
         for i,v in pairs( tables[idx] ) do
            if type( v ) == "table" then
               tables[idx][i] = tables[v[1]]
            end
            if type( i ) == "table" and tables[i[1]] then
               table.insert( tolinki,{ i,tables[i[1]] } )
            end
         end
         -- link indices
         for _,v in ipairs( tolinki ) do
            tables[idx][v[2]],tables[idx][v[1]] =  tables[idx][v[1]],nil
         end
      end
      return tables[1]
   end
-- close do
end

-- ChillCode







-- ,

-- {
-- "hook_id"	: "lib/units/beings/player/states/playerstandard",
 -- "script_path" : "hooks/playerstandard.lua" 
-- }







local mvec3_dis_sq = mvector3.distance_sq
local mvec3_set = mvector3.set
local mvec3_set_z = mvector3.set_z
local mvec3_sub = mvector3.subtract
local mvec3_add = mvector3.add
local mvec3_mul = mvector3.multiply
local mvec3_norm = mvector3.normalize
PlayerStandard = PlayerStandard or class(PlayerMovementState)
PlayerStandard.MOVER_STAND = Idstring("stand")
PlayerStandard.MOVER_DUCK = Idstring("duck")
PlayerStandard.MOVEMENT_DEADZONE = 0.1
PlayerStandard.ANIM_STATES = {
	standard = {
		equip = Idstring("equip"),
		mask_equip = Idstring("mask_equip"),
		unequip = Idstring("unequip"),
		reload_exit = Idstring("reload_exit"),
		reload_not_empty_exit = Idstring("reload_not_empty_exit"),
		start_running = Idstring("start_running"),
		stop_running = Idstring("stop_running"),
		melee = Idstring("melee"),
		melee_miss = Idstring("melee_miss"),
		melee_bayonet = Idstring("melee_bayonet"),
		melee_miss_bayonet = Idstring("melee_miss_bayonet"),
		idle = Idstring("idle"),
		use = Idstring("use"),
		recoil = Idstring("recoil"),
		recoil_steelsight = Idstring("recoil_steelsight"),
		recoil_enter = Idstring("recoil_enter"),
		recoil_loop = Idstring("recoil_loop"),
		recoil_exit = Idstring("recoil_exit"),
		melee_charge = Idstring("melee_charge"),
		melee_charge_state = Idstring("fps/melee_charge"),
		melee_attack = Idstring("melee_attack"),
		melee_attack_state = Idstring("fps/melee_attack"),
		melee_exit_state = Idstring("fps/melee_exit"),
		melee_enter = Idstring("melee_enter"),
		projectile_start = Idstring("throw_projectile_start"),
		projectile_idle = Idstring("throw_projectile_idle"),
		projectile_throw = Idstring("throw_projectile"),
		projectile_throw_state = Idstring("fps/throw_projectile"),
		projectile_exit_state = Idstring("fps/throw_projectile_exit"),
		projectile_enter = Idstring("throw_projectile_enter"),
		charge = Idstring("charge"),
		base = Idstring("base"),
		cash_inspect = Idstring("cash_inspect"),
		falling = Idstring("falling"),
		tased = Idstring("tased"),
		tased_exit = Idstring("tased_exit"),
		tased_boost = Idstring("tased_boost"),
		tased_counter = Idstring("tazer_counter")
	},
	bipod = {
		recoil = Idstring("bipod_recoil"),
		recoil_steelsight = Idstring("bipod_recoil_steelsight"),
		recoil_enter = Idstring("bipod_recoil_enter"),
		recoil_loop = Idstring("bipod_recoil_loop"),
		recoil_exit = Idstring("bipod_recoil_exit")
	},
	underbarrel = {
		equip = Idstring("underbarrel_equip"),
		unequip = Idstring("underbarrel_unequip"),
		start_running = Idstring("underbarrel_start_running"),
		stop_running = Idstring("underbarrel_stop_running"),
		melee = Idstring("underbarrel_melee"),
		melee_miss = Idstring("underbarrel_melee_miss"),
		idle = Idstring("underbarrel_idle")
	}
}
PlayerStandard.projectile_throw_delays = {
	projectile_frag_com = 0.83062744140625,
	projectile_dynamite = 0.86656761169434,
	projectile_molotov = 0.86867332458496,
	projectile_frag = 0.52984619140625
}
PlayerStandard.debug_bipod = nil

local melee_vars = {
	"player_melee",
	"player_melee_var2"
}


function PlayerStandard:_do_melee_damage(t, bayonet_melee, melee_hit_ray, melee_entry, hand_id)
	melee_entry = melee_entry or managers.blackmarket:equipped_melee_weapon()
	local instant_hit = tweak_data.blackmarket.melee_weapons[melee_entry].instant
	local melee_damage_delay = tweak_data.blackmarket.melee_weapons[melee_entry].melee_damage_delay or 0
	local charge_lerp_value = instant_hit and 0 or self:_get_melee_charge_lerp_value(t, melee_damage_delay)

	self._ext_camera:play_shaker(melee_vars[math.random(#melee_vars)], math.max(0.3, charge_lerp_value))

	local sphere_cast_radius = 20
	local col_ray = nil

	if melee_hit_ray then
		col_ray = melee_hit_ray ~= true and melee_hit_ray or nil
	else
		col_ray = self:_calc_melee_hit_ray(t, sphere_cast_radius)
	end

	if col_ray and alive(col_ray.unit) then
		local damage, damage_effect = managers.blackmarket:equipped_melee_weapon_damage_info(charge_lerp_value)
		local damage_effect_mul = math.max(managers.player:upgrade_value("player", "melee_knockdown_mul", 1), managers.player:upgrade_value(self._equipped_unit:base():weapon_tweak_data().categories and self._equipped_unit:base():weapon_tweak_data().categories[1], "melee_knockdown_mul", 1))
		damage = damage * managers.player:get_melee_dmg_multiplier()
		damage_effect = damage_effect * damage_effect_mul
		col_ray.sphere_cast_radius = sphere_cast_radius
		local hit_unit = col_ray.unit

		if hit_unit:character_damage() then
			if bayonet_melee then
				self._unit:sound():play("fairbairn_hit_body", nil, false)
			else
				local hit_sfx = "hit_body"

				if hit_unit:character_damage() and hit_unit:character_damage().melee_hit_sfx then
					hit_sfx = hit_unit:character_damage():melee_hit_sfx()
				end

				self:_play_melee_sound(melee_entry, hit_sfx, self._melee_attack_var)
			end

			if not hit_unit:character_damage()._no_blood then
				managers.game_play_central:play_impact_flesh({
					col_ray = col_ray
				})
				managers.game_play_central:play_impact_sound_and_effects({
					no_decal = true,
					no_sound = true,
					col_ray = col_ray
				})
			end

			self._camera_unit:base():play_anim_melee_item("hit_body")
		elseif self._on_melee_restart_drill and hit_unit:base() and (hit_unit:base().is_drill or hit_unit:base().is_saw) then
			hit_unit:base():on_melee_hit(managers.network:session():local_peer():id())
		else
			if bayonet_melee then
				self._unit:sound():play("knife_hit_gen", nil, false)
			else
				self:_play_melee_sound(melee_entry, "hit_gen", self._melee_attack_var)
			end

			self._camera_unit:base():play_anim_melee_item("hit_gen")
			managers.game_play_central:play_impact_sound_and_effects({
				no_decal = true,
				no_sound = true,
				col_ray = col_ray,
				effect = Idstring("effects/payday2/particles/impacts/fallback_impact_pd2")
			})
		end

		local custom_data = nil

		if _G.IS_VR and hand_id then
			custom_data = {
				engine = hand_id == 1 and "right" or "left"
			}
		end

		managers.rumble:play("melee_hit", nil, nil, custom_data)
		managers.game_play_central:physics_push(col_ray)

		local character_unit, shield_knock = nil
		local can_shield_knock = managers.player:has_category_upgrade("player", "shield_knock")

		if can_shield_knock and hit_unit:in_slot(8) and alive(hit_unit:parent()) and not hit_unit:parent():character_damage():is_immune_to_shield_knockback() then
			shield_knock = true
			character_unit = hit_unit:parent()
		end

		character_unit = character_unit or hit_unit

		if character_unit:character_damage() and character_unit:character_damage().damage_melee then
			local dmg_multiplier = 1

			if not managers.enemy:is_civilian(character_unit) and not managers.groupai:state():is_enemy_special(character_unit) then
				dmg_multiplier = dmg_multiplier * managers.player:upgrade_value("player", "non_special_melee_multiplier", 1)
			else
				dmg_multiplier = dmg_multiplier * managers.player:upgrade_value("player", "melee_damage_multiplier", 1)
			end

			dmg_multiplier = dmg_multiplier * managers.player:upgrade_value("player", "melee_" .. tostring(tweak_data.blackmarket.melee_weapons[melee_entry].stats.weapon_type) .. "_damage_multiplier", 1)

			if managers.player:has_category_upgrade("melee", "stacking_hit_damage_multiplier") then
				self._state_data.stacking_dmg_mul = self._state_data.stacking_dmg_mul or {}
				self._state_data.stacking_dmg_mul.melee = self._state_data.stacking_dmg_mul.melee or {
					nil,
					0
				}
				local stack = self._state_data.stacking_dmg_mul.melee

				if stack[1] and t < stack[1] then
					dmg_multiplier = dmg_multiplier * (1 + managers.player:upgrade_value("melee", "stacking_hit_damage_multiplier", 0) * stack[2])
				else
					stack[2] = 0
				end
			end

			local health_ratio = self._ext_damage:health_ratio()
			local damage_health_ratio = managers.player:get_damage_health_ratio(health_ratio, "melee")

			if damage_health_ratio > 0 then
				local damage_ratio = damage_health_ratio
				dmg_multiplier = dmg_multiplier * (1 + managers.player:upgrade_value("player", "melee_damage_health_ratio_multiplier", 0) * damage_ratio)
			end

			dmg_multiplier = dmg_multiplier * managers.player:temporary_upgrade_value("temporary", "berserker_damage_multiplier", 1)
			local target_dead = character_unit:character_damage().dead and not character_unit:character_damage():dead()
			local target_hostile = managers.enemy:is_enemy(character_unit) and not tweak_data.character[character_unit:base()._tweak_table].is_escort and character_unit:brain():is_hostile()
			local life_leach_available = managers.player:has_category_upgrade("temporary", "melee_life_leech") and not managers.player:has_activate_temporary_upgrade("temporary", "melee_life_leech")

			if target_dead and target_hostile and life_leach_available then
				managers.player:activate_temporary_upgrade("temporary", "melee_life_leech")
				self._unit:character_damage():restore_health(managers.player:temporary_upgrade_value("temporary", "melee_life_leech", 1))
			end

			local special_weapon = tweak_data.blackmarket.melee_weapons[melee_entry].special_weapon
			local action_data = {
				variant = "melee"
			}

			if special_weapon == "taser" then
				action_data.variant = "taser_tased"
			end

			if _G.IS_VR and melee_entry == "weapon" and not bayonet_melee then
				dmg_multiplier = 0.1
			end

			action_data.damage = shield_knock and 0 or damage * dmg_multiplier
			action_data.damage_effect = damage_effect
			action_data.attacker_unit = self._unit
			action_data.col_ray = col_ray

			if shield_knock then
				action_data.shield_knock = can_shield_knock
			end

			action_data.name_id = melee_entry
			action_data.charge_lerp_value = charge_lerp_value

			if managers.player:has_category_upgrade("melee", "stacking_hit_damage_multiplier") then
				self._state_data.stacking_dmg_mul = self._state_data.stacking_dmg_mul or {}
				self._state_data.stacking_dmg_mul.melee = self._state_data.stacking_dmg_mul.melee or {
					nil,
					0
				}
				local stack = self._state_data.stacking_dmg_mul.melee


				-- local mytable = {tweak_data.skilltree.specializations[8][5].upgrades}
				-- table.save(mytable,"C:\\Users\\USER\\Desktop\\payday debug\\debug.txt")
 
 

				if character_unit:character_damage().dead and not character_unit:character_damage():dead() then
					stack[1] = t + managers.player:upgrade_value("melee", "stacking_hit_expire_t", 1)
					stack[2] = math.min(stack[2] + 1, tweak_data.upgrades.max_melee_weapon_dmg_mul_stacks or 5)
				else
					stack[1] = nil
					stack[2] = 0
				end
			end

			local defense_data = character_unit:character_damage():damage_melee(action_data)

			self:_check_melee_dot_damage(col_ray, defense_data, melee_entry)
			self:_perform_sync_melee_damage(hit_unit, col_ray, action_data.damage)

			return defense_data
		else
			self:_perform_sync_melee_damage(hit_unit, col_ray, damage)
		end
	end

	if managers.player:has_category_upgrade("melee", "stacking_hit_damage_multiplier") then
		self._state_data.stacking_dmg_mul = self._state_data.stacking_dmg_mul or {}
		self._state_data.stacking_dmg_mul.melee = self._state_data.stacking_dmg_mul.melee or {
			nil,
			0
		}
		local stack = self._state_data.stacking_dmg_mul.melee
		stack[1] = nil
		stack[2] = 0
	end

	return col_ray
end