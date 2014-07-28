IDRIS := idris

quartz:
	${IDRIS} --build iridium-quartz.ipkg
	${IDRIS} -i src -p effects -o iridium Quartz
