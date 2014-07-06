IDRIS := idris

quartz:
	${IDRIS} --build iridium-quartz.ipkg
	${IDRIS} -i src -p neweffects -o iridium Quartz
