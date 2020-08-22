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




-- local mytable = {}


-- function UpgradesManager:aquire(id, loading, identifier)
	


	
	-- table.insert(mytable, id)
	-- table.save(mytable,"C:\\Users\\USER\\Desktop\\payday debug\\debug.txt")
 
 
 
	
	-- if not tweak_data.upgrades.definitions[id] then
		-- Application:error("Tried to aquire an upgrade that doesn't exist: " .. (id or "nil") .. "")

		-- return
	-- end

	-- local upgrade = tweak_data.upgrades.definitions[id]

	-- if upgrade.dlc and not managers.dlc:is_dlc_unlocked(upgrade.dlc) then
		-- Application:error("Tried to aquire an upgrade locked to a dlc you do not have: " .. id .. " DLC: ", upgrade.dlc)

		-- return
	-- end

	-- if not identifier then
		-- debug_pause(identifier, "[UpgradesManager:aquire] No identifier for upgrade aquire", "id", id, "loading", loading)

		-- identifier = UpgradesManager.AQUIRE_STRINGS[1]
	-- end

	-- local identify_key = Idstring(identifier):key()

	-- if self._global.aquired[id] and self._global.aquired[id][identify_key] then
		-- Application:error("Tried to aquire an upgrade that has already been aquired: " .. id, "identifier", identifier, "id_key", identify_key)
		-- Application:stack_dump()

		-- return
	-- end

	-- self._global.aquired[id] = self._global.aquired[id] or {}
	-- self._global.aquired[id][identify_key] = identifier

	-- self:_aquire_upgrade(upgrade, id, loading)
	-- self:setup_current_weapon()
-- end
-- local original_init = SkillTreeTweakData.init

-- function SkillTreeTweakData:init(...)
	-- original_init(self, ...)
	
	-- table.insert(self.specializations[8][5].upgrades, "player_overdog_expire_t")

	-- local mytable = {self.specializations[8][5]}
	-- table.save(mytable,"C:\\Users\\USER\\Desktop\\payday debug\\debug.txt")
 
	
-- end