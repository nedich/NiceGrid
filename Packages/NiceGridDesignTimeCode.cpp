//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <Dialogs.Hpp>

#include <windows.h>

//
// Includes to support design-time code
//
#include <DsgnIntf.hpp>
#include <ToolsApi.hpp>
#include <ColnEdit.hpp>

#include "NiceGrid.hpp"
#include "NiceGridDesignTimeCode.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)


//---------------------------------------------------------------------------

void __fastcall TNiceGridComponentEditor::ExecuteVerb(int Index)
{
  switch (Index)
  {
    case 0: //ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
            //                          TNiceGrid(Component).Columns, 'Columns', [coAdd, coDelete, coMove]);
            ShowCollectionEditorClass(Designer, __classid(TCollectionEditor), Component,
                                      ((TNiceGrid*)Component)->Columns, "Columns", TColOptions() << coAdd << coDelete << coMove);
            break;

    case 1: ShowMessage(AnsiString("TNiceGrid v 2.10 (Mozilla Public License)\n")
                      + AnsiString("(c) Priyatna Harun, 2003\n")
                      + AnsiString("Bandung - Indonesia\n")
                      + AnsiString("http://www.ujangpri.com/\n")
                      + AnsiString("mailto:me@ujangpri.com\n")
                      + AnsiString("\n\n")
                      + AnsiString("C++ Builder 5 support added by C.S. Phua\n")
                      + AnsiString("Petaling Jaya - Malaysia\n")
                      + AnsiString("csphua@teledynamics.com.my\n")
                      );
            break;
  }
}
//---------------------------------------------------------------------------

AnsiString __fastcall TNiceGridComponentEditor::GetVerb(int Index)
{
  switch (Index)
  {
    case 0: return "Edit Columns ...";
    case 1: return "About";
  }
}
//---------------------------------------------------------------------------

int __fastcall TNiceGridComponentEditor::GetVerbCount()
{
  return 2;
}
//---------------------------------------------------------------------------


