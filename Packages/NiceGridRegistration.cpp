#include <vcl.h>
#pragma hdrstop

#include <TypInfo.hpp>

#include "NiceGrid.hpp"
#include "NiceGridDesignTimeCode.h"
#include "NiceGridRegistration.h"

#define NDEBUG
#include <cassert>

#pragma package(smart_init)

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// Registration code for design-time interaction
//

namespace Nicegridregistration
{
  void __fastcall PACKAGE Register(void)
  {
    const AnsiString ComponentPaletteLocation = "Nice Grid";
    //
    // Register the components on the component palette
    //
    const int NumberCoreClasses = 1;
    TComponentClass CoreClass[NumberCoreClasses] = { __classid(TNiceGrid) };
    RegisterComponents(ComponentPaletteLocation, CoreClass, NumberCoreClasses - 1);

    //
    // Register a component editor (for double-clicking on the component)
    //
    RegisterComponentEditor( __classid(TNiceGrid),
                             __classid(TNiceGridComponentEditor) );

    //
    // Install design-time hook for enabling VERSIONINFO resources
    // in the IDE via the Open Tools API interface.
    //
    #if 0
      // This code is currently disabled, since
      // enabling the Version Information does
      // not work from design-time code.
      DesignTimeHook = EnableVersionInfoInIDE;
    #endif
  }
}