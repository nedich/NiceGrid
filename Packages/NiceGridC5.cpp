//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("NiceGridC5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("NiceGrid.pas");
USERES("NiceGrid.dcr");
USEPACKAGE("dsnide50.bpi");
USEUNIT("NiceGridDesignTimeCode.cpp");
USEUNIT("NiceGridRegistration.cpp");
//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
