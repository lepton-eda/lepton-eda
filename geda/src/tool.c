/* $Id$ */

/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@eter.ariadna.pl                   */
/*                                                                             */
/* This program is free software; you can redistribute it and/or               */
/* modify it under the terms of the GNU General Public License                 */
/* as published by the Free Software Foundation version 2.                     */
/*                                                                             */
/* This program is distributed in the hope that it will be useful,             */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               */
/* GNU General Public License for more details.                                */
/*                                                                             */
/* You should have received a copy of the GNU General Public License           */
/* along with this program; if not, write to the Free Software                 */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
/*                                                                             */
/*******************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <ctype.h>
#include <gtk/gtk.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/wait.h>

#ifdef HAVE_UNISTD_H  
#include <unistd.h>
#endif

#include "configfile.h"
#include "doc.h"
#include "file.h"
#include "filetool.h"
#include "global.h"
#include "log.h"
#include "m_action.h"
#include "m_window.h"
#include "msgbox.h"
#include "support.h"
#include "task.h"
#include "tool.h"



/*
	Groups of files
*/

struct Group_s
{
	int iId;
	char szName[TEXTLEN];
	GtkCTreeNode *pNode;
	struct Group_s *pNext;
};



/*
	Definitions of databases
*/

/* known tools */
enum {
	TOOL_EDIT = 0,
	TOOL_GSCHEM,
	TOOL_GSCHEM2PCB,
	TOOL_GNETLIST,
	TOOL_PCB,
	TOOL_GERBV,
	TOOL_EASYSPICE,
	TOOL_NGSPICE,
	TOOL_GWAVE,
	TOOL_VC,
	TOOL_VVP,
	TOOL_GTKWAVE,
	TOOL_NUMBER
};

/* known groups */
enum {
	GROUP_DOCUMENTATION = 0,
	GROUP_SCHEMATIC,
	GROUP_LAYOUT,
	GROUP_SIMULATION,
	GROUP_GRAPH,
	GROUP_PROGRAM,
	GROUP_NUMBER
};

/* known actions */
enum {
	ACTION_TXT_OPEN = 0,
	ACTION_SCH_OPEN,
	ACTION_SCH_TO_PCB,
	ACTION_SCH_TO_CIR,
	ACTION_SCH_TO_V,
	ACTION_SCH_TO_VHD,
	ACTION_SCH_TO_DRC,
	ACTION_SCH_TO_BOM,
	ACTION_PCB_OPEN,
	ACTION_GRB_OPEN,
	ACTION_ESP_OPEN,
	ACTION_CIR_OPEN,
	ACTION_CIR_TO_OUT,
	ACTION_OUT_OPEN,
	ACTION_GW_OPEN,
	ACTION_V_OPEN,
	ACTION_V_TO_VCD,
	ACTION_V_TO_XNF,
	ACTION_VCD_OPEN,
	ACTION_VHD_OPEN,
	ACTION_DRC_OPEN,
	ACTION_BOM_OPEN,
	ACTION_SYM_OPEN,
	ACTION__OPEN,
	ACTION_NUMBER
};

/* known extensions */
enum {
	EXT_TXT = 0,
	EXT_SCH,
	EXT_PCB,
	EXT_GRB,
	EXT_ESP,
	EXT_CIR,
	EXT_OUT,
	EXT_GW,
	EXT_V,
	/* Commenting out this enum as it is not used anywhere else */
	/* and it seemed to cause a crash for certain users.  Identified by */
  	/* Werner Hoch */	
	/* EXT_VVP, */  
	EXT_VCD,
	EXT_VHD,
	EXT_DRC,
	EXT_BOM,
	EXT_SYM,
	EXT_,
	EXT_NUMBER
};



#define TOOL_TEMPLATE_TXT    ""
#define TOOL_TEMPLATE_SCH    ""
#define TOOL_TEMPLATE_PCB    ""
#define TOOL_TEMPLATE_CIR    "<SIMULATION NAME>\n\n<CIRCUIT DESCRIPTION>\n\n<ANALYSIS DESCRIPTION>\n"
#define TOOL_TEMPLATE_V      ""
#define TOOL_TEMPLATE_VHD    ""



/* tool types (Tools.iType) */
#define TOOL_EMBEDDED        1
#define TOOL_EXTERNAL        2



/* tool entries */
static struct Tool_s ToolList[] = 
{
	{ TOOL_EDIT,       "Text editor",                 "-----",      TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GSCHEM,     "Schematic editor",            "gschem",     TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GSCHEM2PCB, "",                            "gschem2pcb", TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GNETLIST,   "",                            "gnetlist",   TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_PCB,        "PCB layout editor",           "pcb",        TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GERBV,      "Gerber viewer",               "gerbv",      TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_EASYSPICE,  "Easy SPICE",                  "easy_spice", TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_NGSPICE,    "",                            "ng-spice",   TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GWAVE,      "Analog waveform viewer",      "gwave",      TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_VC,         "",                            "iverilog",   TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_VVP,        "",                            "vvp",        TOOL_EXTERNAL, NULL, NULL },
	{ TOOL_GTKWAVE,    "",                            "gtkwave",    TOOL_EXTERNAL, NULL, NULL }
};

/* group entries */
struct Group_s GroupList[] =
{
	{ GROUP_DOCUMENTATION, "Documentations",       NULL, NULL },
	{ GROUP_SCHEMATIC,     "Schematic diagrams",   NULL, NULL },
	{ GROUP_LAYOUT,        "PCB and VLSI layouts", NULL, NULL },
	{ GROUP_SIMULATION,    "Simulations",          NULL, NULL },
	{ GROUP_GRAPH,         "Graphs",               NULL, NULL },
	{ GROUP_PROGRAM,       "Chip programs",        NULL, NULL }
};

/* action entries */
struct Action_s ActionList[] = 
{
	{
		ACTION_TXT_OPEN,
		"Open",
		"txt",
		"----- %FILEREL%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION__OPEN,
		"Open",
		"",
		"----- %FILEREL%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION_SCH_OPEN,
		"Open",
		"sch",
		"gschem %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_GSCHEM,
		NULL,
		0,
		NULL
	},

	{
		ACTION_SYM_OPEN,
		"Open",
		"sym",
		"gschem %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_GSCHEM,
		NULL,
		0,
		NULL
	},

	{
		ACTION_SCH_TO_PCB,
		"Create PCB layout",
		"sch",
		"gnetlist -g PCBboard -o %FILENAME%.pcb %FILENAME%.%FILEEXT%",
		",%FILENAME%.pcb",
		TASK_BLOCKING,
		TOOL_GSCHEM2PCB,
		NULL,
		0,
		NULL
	},

	{
		ACTION_SCH_TO_CIR,
		"Create SPICE netlist",
		"sch",
		"gnetlist -g spice -o %FILENAME%.cir %FILENAME%.%FILEEXT%",
		",%FILENAME%.cir",
		TASK_BLOCKING,
		TOOL_GNETLIST,
		NULL,
		0,
		NULL
	},

 	{
		ACTION_SCH_TO_V,
		"Create Verilog source",
		"sch",
		"gnetlist -g verilog -o %FILENAME%.v %FILENAME%.%FILEEXT%",
		",%FILENAME%.v",
		TASK_BLOCKING,
		TOOL_GNETLIST,
		NULL,
		0,
		NULL
	},

 	{
		ACTION_SCH_TO_VHD,
		"Create VHDL source",
		"sch",
		"gnetlist -g vhdl -o %FILENAME%.vhd %FILENAME%.%FILEEXT%",
		",%FILENAME%.vhd",
		TASK_BLOCKING,
		TOOL_GNETLIST,
		NULL,
		0,
		NULL
	},

 	{
		ACTION_SCH_TO_DRC,
		"Design rule checking (DRC)",
		"sch",
		"gnetlist -g drc -o %FILENAME%.drc %FILENAME%.%FILEEXT%",
		"%FILENAME%.%FILEEXT%,%FILENAME%.drc",
		TASK_BLOCKING,
		TOOL_GNETLIST,
		NULL,
		0,
		NULL
	},

 	{
		ACTION_SCH_TO_BOM,
		"Create bill of materials (BOM)",
		"sch",
		"gnetlist -g bom -o %FILENAME%.bom %FILENAME%.%FILEEXT%",
		"%FILENAME%.%FILEEXT%,%FILENAME%.bom",
		TASK_BLOCKING,
		TOOL_GNETLIST,
		NULL,
		0,
		NULL
	},

	{
		ACTION_PCB_OPEN,
		"Open",
		"pcb",
		"pcb %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_PCB,
		NULL,
		0,
		NULL
	},

	{
		ACTION_GRB_OPEN,
		"Open",
		"grb",
		"gerbv %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_GERBV,
		NULL,
		0,
		NULL
	},

	{
		ACTION_ESP_OPEN,
		"Open",
		"esp",
		"easy_spice -f %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_EASYSPICE,
		NULL,
		0,
		NULL
	},

	{
		ACTION_CIR_OPEN,
		"Open",
		"cir",
		"----- %FILENAME%.%FILEEXT%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION_CIR_TO_OUT,
		"Simulate (SPICE)",
		"cir",
		"ngspice -b -i %FILENAME%.%FILEEXT% -r %FILENAME%.out; raw2gw %FILENAME%.out %FILENAME%.gw",
		"%FILENAME%.%FILEEXT%,%FILENAME%.out,%FILENAME%.out,%FILENAME%.gw",
		TASK_BLOCKING,
		TOOL_NGSPICE,
		NULL,
		0,
		NULL
	},

	{
		ACTION_OUT_OPEN,
		"Open",
		"out",
		"raw2gw %FILENAME%.%FILEEXT% %FILENAME.gw ; gwave -s %FILENAME%.gw",
		"%FILENAME%.out,%FILENAME%.gw",
		TASK_NOFLAG,
		TOOL_GWAVE,
		NULL,
		0,
		NULL
	},

	{
		ACTION_GW_OPEN,
		"Open",
		"gw",
		"gwave -s %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_GWAVE,
		NULL,
		0,
		NULL
	},

	{
		ACTION_V_OPEN,
		"Open",
		"v",
		"----- %FILENAME%.%FILEEXT%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION_V_TO_VCD,
		"Simulate",
		"v",
		"iverilog -o %FILENAME%.vvp -t vvp %FILENAME%.%FILEEXT% ; vvp %FILENAME%.vvp",
		"%FILENAME%.v,%FILENAME%.vcd",
		TASK_BLOCKING,
		TOOL_VC,
		NULL,
		0,
		NULL
	},

	{
		ACTION_V_TO_XNF,
		"Synthesize",
		"v",
		"iverilog -o %FILENAME%.xnf -t xnf %FILENAME%.%FILEEXT%",
		"%FILENAME%.%FILEEXT%,%FILENAME%.xnf",
		TASK_BLOCKING,
		TOOL_VVP,
		NULL,
		0,
		NULL
	},

	{
		ACTION_VCD_OPEN,
		"Open",
		"vcd",
		"gtkwave %FILENAME%.%FILEEXT%",
		"",
		ACTION_DEFAULT,
		TOOL_GTKWAVE,
		NULL,
		0,
		NULL
	},

	{
		ACTION_VHD_OPEN,
		"Open",
		"vhd",
		"----- %FILEREL%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION_DRC_OPEN,
		"Open",
		"drc",
		"----- %FILEREL%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

	{
		ACTION_BOM_OPEN,
		"Open",
		"bom",
		"----- %FILEREL%",
		"",
		TASK_INTERNAL | ACTION_DEFAULT,
		TOOL_EDIT,
		NULL,
		0,
		NULL
	},

/*
gnetlist [-i] [-v] [-r rcfilename] [-g guile_procedure] [-o output_filename] schematic1 [... schematicN]

        1) PCB / PCBboard (-g PCB and -g PCBboard)
        2) Allegro netlist format (-g allegro)
        3) BAE netlist format (-g bae)
        4) BOM / BOM2 - Bill of Materials (-g bom and -g bom2)
        5) DRC - Start of a design rule checker (-g drc)
        6) gEDA - native format, mainly used for testing (-g geda)
        7) Gossip netlist format (-g gossip)
        8) PADS netlist format (-g pads)
        9) ProtelII netlist format (-g protelII)
        10) Spice compatible netlist format (-g spice)
        11) Tango netlist format (-g tango)
        12) Verilog code (-g verilog)
        13) VHDL code (-g vhdl)
        14) VIPEC netlist format (-g vipec)	

*/
};

/* extension entries */
static struct Ext_s ExtList[] = 
{
	{ EXT_TXT, "Text",             "txt", "",    GROUP_DOCUMENTATION, TRUE,  FALSE, TOOL_TEMPLATE_TXT, ACTION_TXT_OPEN, NULL, NULL },
	{ EXT_SCH, "Schematic",        "sch", "",    GROUP_SCHEMATIC,     TRUE,  FALSE, TOOL_TEMPLATE_SCH, ACTION_SCH_OPEN, NULL, NULL },
	{ EXT_PCB, "PCB layout",       "pcb", "",    GROUP_LAYOUT,        TRUE,  FALSE, TOOL_TEMPLATE_PCB, ACTION_PCB_OPEN, NULL, NULL },
	{ EXT_GRB, "Gerber data",      "grb", "pcb", GROUP_LAYOUT,        FALSE, FALSE, "",                ACTION_GRB_OPEN, NULL, NULL },
	{ EXT_ESP, "Simulation",       "esp", "",    GROUP_SIMULATION,    TRUE,  FALSE, "",                ACTION_ESP_OPEN, NULL, NULL },
	{ EXT_CIR, "SPICE netlist",    "cir", "esp", GROUP_SIMULATION,    TRUE,  FALSE, TOOL_TEMPLATE_CIR, ACTION_CIR_OPEN, NULL, NULL },
	{ EXT_OUT, "",                 "out", "cir", GROUP_SIMULATION,    FALSE, FALSE, "",                ACTION_OUT_OPEN, NULL, NULL },
	{ EXT_GW,  "Analog waveform",  "gw",  "out", GROUP_GRAPH,         TRUE,  FALSE, "",                ACTION_GW_OPEN,  NULL, NULL },
	{ EXT_V,   "Verilog source",   "v",   "",    GROUP_PROGRAM,       TRUE,  FALSE, TOOL_TEMPLATE_V,   ACTION_V_OPEN,   NULL, NULL },
	{ EXT_VCD, "",                 "vcd", "v",   GROUP_GRAPH,         FALSE, FALSE, "",                ACTION_VCD_OPEN, NULL, NULL },
	{ EXT_VHD, "VHDL source",      "vcd", "vhd", GROUP_PROGRAM,       TRUE,  FALSE, TOOL_TEMPLATE_VHD, ACTION_VHD_OPEN, NULL, NULL },
	{ EXT_DRC, "",                 "drc", "sch", GROUP_SCHEMATIC,     FALSE, FALSE, "",                ACTION_DRC_OPEN, NULL, NULL },
	{ EXT_BOM, "",                 "bom", "sch", GROUP_SCHEMATIC,     FALSE, FALSE, "",                ACTION_BOM_OPEN, NULL, NULL },
	{ EXT_SYM, "Schematic symbol", "sym", "",    GROUP_SCHEMATIC,     TRUE,  FALSE, "",                ACTION_SYM_OPEN, NULL, NULL },
	{ EXT_,    "Description",      "",    "",    GROUP_DOCUMENTATION, FALSE, FALSE, "",                ACTION__OPEN,    NULL, NULL }
};



/* tool structures */
static struct Tool_s *pToolList = NULL;
struct Action_s *pActionList = NULL;
struct Ext_s *pExtList = NULL;
struct Group_s *pGroupList = NULL;



/* static functions */
static int ToolReadTools(void);
static int ToolRegisterTool(struct Tool_s Entry);
static int ToolReadActions(void);
static int ToolRegisterAction(struct Action_s Entry);
static int ToolReadGroups(void);
static int ToolRegisterGroup(struct Group_s Entry);
static int ToolReadExtensions(void);
static int ToolRegisterExtension(struct Ext_s Entry);
static int Menu_Tool_Initialize(void);
static void Menu_Tool_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void StrReplace(char *szString, char *szFrom, char *szTo);




/*
	Initializing and releasing databases
*/

int ToolInitialize(void)
{
	int iResult;

	/* read table of tools */
	iResult = ToolReadTools();
	if (iResult == FAILURE)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	/* read table of groups */
	iResult = ToolReadGroups();
	if (iResult == FAILURE)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	/* read table of actions */
	iResult = ToolReadActions();
	if (iResult == FAILURE)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	/* read table of extensions */
	iResult = ToolReadExtensions();
	if (iResult == FAILURE)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	/* menu modification */
	iResult = Menu_Tool_Initialize();
	if (iResult == FAILURE)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	MenuActionInitialize();
	MenuWindowInitialize();

	return SUCCESS;
}


int ToolRelease()
{
	return SUCCESS;
}


/*
	Reading fields from extension database
*/

/* read database fields */
int ToolValueGet(int iDataBase, int iValue, int iId, void *pValue)
{
	struct Tool_s *pTool;
	struct Action_s *pAction;
	struct Ext_s *pExt;
	struct Group_s *pGroup;

	switch (iDataBase)
	{
		case TOOL_LIST:

			for (pTool = pToolList; pTool != NULL; pTool = pTool->pNext)
				if (pTool->iId == iId)
					break;
			if (pTool == NULL)
				return FAILURE;

			switch (iValue)
			{
				case TOOL_ID:        *((int *) pValue) = pTool->iId;              break;
				case TOOL_NAME:      strcpy((char *) pValue, pTool->szName);      break;
				case TOOL_COMMAND:   strcpy((char *) pValue, pTool->szCommand);   break;
				case TOOL_TYPE:      *((int *) pValue) = pTool->iType;            break;
				default:                  return FAILURE;
			}

			break;

		case ACTION_LIST:

			for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
				if (pAction->iId == iId)
					break;
			if (pAction == NULL)
				return FAILURE;

			switch (iValue)
			{
				case ACTION_ID:      *((int *) pValue) = pAction->iId;            break;
				case ACTION_NAME:    strcpy((char *) pValue, pAction->szName);    break;
				case ACTION_EXT:     strcpy((char *) pValue, pAction->szExt);     break;
				case ACTION_COMMAND: strcpy((char *) pValue, pAction->szCommand); break;
				case ACTION_IMPORT:  strcpy((char *) pValue, pAction->szImport);  break;
				case ACTION_TOOLID:  *((int *) pValue) = pAction->iToolId;        break;
				default:                  return FAILURE;
			}

			break;

		case EXT_LIST:

			for (pExt = pExtList; pExt != NULL; pExt = pExt->pNext)
				if (pExt->iId == iId)
					break;
			if (pExt == NULL)
				return FAILURE;

			switch (iValue)
			{
				case EXT_ID:         *((int *) pValue) = pExt->iId;               break;
				case EXT_NAME:       strcpy((char *) pValue, pExt->szName);       break;
				case EXT_EXT:        strcpy((char *) pValue, pExt->szExt);        break;
				case EXT_INFILENEW:  *((int *) pValue) = pExt->bInFileNew;        break;
				case EXT_TEMPLATE:   strcpy((char *) pValue, pExt->szTemplate);   break;
				case EXT_ACTIONID:   *((int *) pValue) = pExt->iActionId;         break;
				case EXT_GROUPID:    *((int *) pValue) = pExt->iGroupId;          break;
				case EXT_PARENT:     strcpy((char *) pValue, pExt->szParent);     break;
				default:                  return FAILURE;
			}

			break;

		case GROUP_LIST:

			/* find the specified group */
			for (pGroup = pGroupList; pGroup != NULL; pGroup = pGroup->pNext)
			{
				if (pGroup->iId == iId)
					break;
			}

			/* read the specified variable */
			switch (iValue)
			{
				case GROUP_ID:
					if (pGroup == NULL)
						return FAILURE;
					*((int *) pValue) = pGroup->iId;
					break;

				case GROUP_NAME:
					if (pGroup == NULL)
						return FAILURE;
					strcpy((char *) pValue, pGroup->szName);
					break;

				case GROUP_NODE:
					if (pGroup == NULL)
						return FAILURE;
					*((GtkCTree **) pValue) = (void *) pGroup->pNode;
					break;

				case GROUP_NEXT:
					if (iId == GROUP_NONE)
					{
						*((int *) pValue) = (pGroupList != NULL) ? pGroupList->iId : GROUP_NONE;
						break;
					}
					if (pGroup == NULL)
						return FAILURE;
					*((int *) pValue) = (pGroup->pNext != NULL) ? pGroup->pNext->iId : GROUP_NONE;
					break;

				default:
					return FAILURE;
			}

			break;

		default:

			return FAILURE;
	}

	return SUCCESS;
}


int ToolValueSet(int iDataBase, int iValue, int iId, void *pValue)
{
	struct Tool_s *pTool;
	struct Action_s *pAction;
	struct Ext_s *pExt;
	struct Group_s *pGroup;

	switch (iDataBase)
	{
		case TOOL_LIST:

			for (pTool = pToolList; pTool != NULL; pTool = pTool->pNext)
				if (pTool->iId == iId)
					break;
			if (pTool == NULL)
				return FAILURE;

			switch (iValue)
			{
			}

			break;

		case ACTION_LIST:

			for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
				if (pAction->iId == iId)
					break;
			if (pAction == NULL)
				return FAILURE;

			switch (iValue)
			{
			}

			break;

		case EXT_LIST:

			for (pExt = pExtList; pExt != NULL; pExt = pExt->pNext)
				if (pExt->iId == iId)
					break;
			if (pExt == NULL)
				return FAILURE;

			switch (iValue)
			{
			}

			break;

		case GROUP_LIST:

			for (pGroup = pGroupList; pGroup != NULL; pGroup = pGroup->pNext)
				if (pGroup->iId == iId)
					break;
			if (pGroup == NULL)
				return FAILURE;

			switch (iValue)
			{
				case GROUP_ID:
					pGroup->iId = *((int *) pValue);
					break;

				case GROUP_NAME:
					strcpy(pGroup->szName, (char *) pValue);
					break;

				case GROUP_NODE:
					pGroup->pNode = *((GtkCTreeNode **) pValue);
					break;

				default:
					return FAILURE;
			}

			break;

		default:

			return FAILURE;
	}

	return SUCCESS;
}



int ToolGetExtensionId(char *szExtension, int *iExtId)
{
	struct Ext_s *pExt;

	/* look for extension entry */
	for (pExt = pExtList; pExt != NULL; pExt = pExt->pNext)
		if (strcmp(pExt->szExt, szExtension) == 0)
			break;
	if (pExt == NULL)
		return FAILURE;
	*iExtId = pExt->iId;

	return SUCCESS;
}


int ToolOpenFile(int iExtId, char *szFilename, int iActionId)
{
	struct Ext_s *pExt;
	struct Action_s *pAction;
	int iResult, i, j;
	char szCommand[TEXTLEN], szFullName[TEXTLEN];

	/* look for extension entry */
	for (pExt = pExtList; pExt != NULL; pExt = pExt->pNext)
	{
		if (pExt->iId == iExtId)
			break;
	}
	if (pExt == NULL)
	{
		Log(LOG_FATAL, __FILE__, __LINE__, "Cannot find extension entry");
		return FAILURE;
	}

	/* if default action - find it in the table */
	if (iActionId == TOOL_DEFAULT)
	{
		/* look for action entry */
		for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
		{
			if (pAction->iId == pExt->iActionId)
				break;
		}
		if (pAction == NULL)
		{
			Log(LOG_FATAL, __FILE__, __LINE__, "Cannot find default action entry");
			return FAILURE;
		}
	}

	/* else find action entry at once */
	else
	{
		for (pAction = pActionList; pAction != NULL; pAction = pAction->pNext)
			if (pAction->iId == iActionId)
				break;
		if (pAction == NULL)
		{
			Log(LOG_FATAL, __FILE__, __LINE__, "Cannot find action entry");
			return FAILURE;
		}
	}

	/* get action command, fill variable fields */
	strcpy(szCommand, pAction->szCommand);
	strcpy(szFullName, szFilename);
	if (strlen(szFilename) > 0)
	{
/*
		strcat(szFullName, ".");
		strcat(szFullName, pExt->szExt);
*/
	}
	for (i = 0; i < strlen(szCommand); i ++)
		if (szCommand[i] == '%')
			break;
	if (i < strlen(szCommand))
	{
		for (j = strlen(szCommand); j > i; j --)
			szCommand[j + strlen(szFullName)] = szCommand[j];
		strcpy(szCommand + i, szFullName);
	}

	/* run action command */
	iResult = ToolRun(szCommand, szFullName, pAction->fFlags);
	if (iResult != SUCCESS)
	{
		/* TODO: error handling */
		return FAILURE;
	}

	return SUCCESS;
}



static int ToolReadTools(void)
{
	int i;

	for (i = 0; i < TOOL_NUMBER; i ++)
		ToolRegisterTool(ToolList[i]);

	return SUCCESS;
}



static int ToolRegisterTool(struct Tool_s Entry)
{
	struct Tool_s *pPtr, *pNewEntry;

	/* a new entry */
	pNewEntry = (struct Tool_s *) malloc(sizeof(struct Tool_s));
	if (pNewEntry == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	pNewEntry->iId = Entry.iId;
	strcpy(pNewEntry->szName, Entry.szName);
	strcpy(pNewEntry->szCommand, Entry.szCommand);
	pNewEntry->iType = Entry.iType;
	pNewEntry->pNext = NULL;

	/* link to a list */
	if (pToolList != NULL)
	{
		for (pPtr = pToolList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pNewEntry;
	}
	else
		pToolList = pNewEntry;

	return SUCCESS;
}



static int ToolReadActions(void)
{
	int i;
	
	for (i = 0; i < ACTION_NUMBER; i ++)
		ToolRegisterAction(ActionList[i]);

	return SUCCESS;
}



static int ToolRegisterAction(struct Action_s Entry)
{
	struct Action_s *pPtr, *pNewEntry;
	
	/* a new entry */
	pNewEntry = (struct Action_s *) malloc(sizeof(struct Action_s));
	if (pNewEntry == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	pNewEntry->iId = Entry.iId;
	strcpy(pNewEntry->szExt, Entry.szExt);
	strcpy(pNewEntry->szName, Entry.szName);
	strcpy(pNewEntry->szCommand, Entry.szCommand);
	strcpy(pNewEntry->szImport, Entry.szImport);
	pNewEntry->fFlags = Entry.fFlags;
	pNewEntry->iToolId = Entry.iToolId;
	pNewEntry->pNext = NULL;

	/* link to a list */
	if (pActionList != NULL)
	{
		for (pPtr = pActionList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pNewEntry;
	}
	else
		pActionList = pNewEntry;

	return SUCCESS;
}



static int ToolReadExtensions(void)
{
	int i;

	for (i = 0; i < EXT_NUMBER; i ++)
		ToolRegisterExtension(ExtList[i]);

	return SUCCESS;

}



static int ToolRegisterExtension(struct Ext_s Entry)
{
	struct Ext_s *pPtr, *pNewEntry;

	/* a new entry */
	pNewEntry = (struct Ext_s *) malloc(sizeof(struct Ext_s));
	if (pNewEntry == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	pNewEntry->iId = Entry.iId;
	strcpy(pNewEntry->szExt, Entry.szExt);
	strcpy(pNewEntry->szParent, Entry.szParent);
	strcpy(pNewEntry->szName, Entry.szName);
	pNewEntry->bInFileNew = Entry.bInFileNew;
	pNewEntry->bMenuUsed = FALSE;
	strcpy(pNewEntry->szTemplate, Entry.szTemplate);
	pNewEntry->iGroupId = Entry.iGroupId;
	pNewEntry->iActionId = Entry.iActionId;
	pNewEntry->pNext = NULL;

	/* link to a list */
	if (pExtList != NULL)
	{
		for (pPtr = pExtList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pNewEntry;
	}
	else
		pExtList = pNewEntry;

	return SUCCESS;
}



static int ToolReadGroups(void)
{
	int i;

	for (i = 0; i < GROUP_NUMBER; i ++)
		ToolRegisterGroup(GroupList[i]);

	return SUCCESS;

}



static int ToolRegisterGroup(struct Group_s Entry)
{
	struct Group_s *pPtr, *pNewEntry;

	/* a new entry */
	pNewEntry = (struct Group_s *) malloc(sizeof(struct Group_s));
	if (pNewEntry == NULL)
	{
		/* TODO: error handling */
		return FAILURE;
	}
	pNewEntry->iId = Entry.iId;
	strcpy(pNewEntry->szName, Entry.szName);
	pNewEntry->pNext = NULL;

	/* link to a list */
	if (pGroupList != NULL)
	{
		for (pPtr = pGroupList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pNewEntry;
	}
	else
		pGroupList = pNewEntry;

	return SUCCESS;
}



/*
	Menu Tool
*/

static int Menu_Tool_Initialize()
{
	GtkWidget *pWidget;
	GtkMenu *pMenu;
	struct Tool_s *pTool;

	/* look for Tool widget */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuTool");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, __FILE__, __LINE__, "Cannot find widget 'MenuTool'");
		return FAILURE;
	}

	/* create Tool menu */
	pMenu = GTK_MENU(gtk_menu_new());
	gtk_widget_ref(GTK_WIDGET(pMenu));
	gtk_object_set_data_full(GTK_OBJECT(pWindowMain), "MenuTool_menu", pMenu, (GtkDestroyNotify) gtk_widget_unref);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (pWidget), GTK_WIDGET(pMenu));

	/* add items to the menu */
	for (pTool = pToolList; pTool != NULL; pTool = pTool->pNext)
	{
		if (strlen(pTool->szName) == 0)
			continue;

		pTool->pMenuItem = GTK_MENU_ITEM(gtk_menu_item_new_with_label(pTool->szName));
		gtk_widget_ref(GTK_WIDGET(pTool->pMenuItem));
		gtk_widget_show(GTK_WIDGET(pTool->pMenuItem));
		gtk_container_add(GTK_CONTAINER(pMenu), GTK_WIDGET(pTool->pMenuItem));
		gtk_signal_connect(GTK_OBJECT(pTool->pMenuItem), "activate", GTK_SIGNAL_FUNC(Menu_Tool_Activation), NULL);
	}

	return SUCCESS;
}


static void Menu_Tool_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	struct Tool_s *pTool;
	int iResult;

	/* look for a tool */
	for (pTool = pToolList; pTool != NULL; pTool = pTool->pNext)
		if (pTool->pMenuItem == pMenuItem)
			break;
	if (pTool == NULL)
	{
		/* TODO: error handling */
		return;
	}

	/* run the tool */
	iResult = ToolRun(pTool->szCommand, "", TASK_NOFLAG);
	if (iResult != SUCCESS)
	{
		/* TODO: error handling */
		return;
	}
}



/****************************************************************************************************/


int ToolRun(char *szCommand, char *szFilename, DWORD fFlags)
{
	pid_t Pid;


	/* create a subtask to run a command */
	Pid = fork();
	if (Pid < 0)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot create a subtask !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return FAILURE;
	}

	/* run command in child process */
	else if (Pid == 0)
	{
		system(szCommand);
		_exit(0);
	}

	/* register the subtask in parent process */
	else
	{
//		TaskNew(Pid, szCommand, fFlags);
	}

	return SUCCESS;
}
