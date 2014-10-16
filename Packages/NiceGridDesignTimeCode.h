//---------------------------------------------------------------------------

#ifndef NiceGridDesignTimeCodeH
#define NiceGridDesignTimeCodeH
//---------------------------------------------------------------------------

#include <DsgnIntf.hpp>

class PACKAGE TNiceGridComponentEditor : public TComponentEditor
{
private:
        typedef TComponentEditor inherited;

//protected:
//        Dsgnintf::_di_IFormDesigner* DesignForm;

public :
//        __fastcall virtual TNiceGridComponentEditor(Classes::TComponent* AComponent,
//                                    _di_IFormDesigner ADesigner);
//        __fastcall virtual ~TNiceGridComponentEditor();

        virtual void __fastcall ExecuteVerb(int Index);
        virtual AnsiString __fastcall GetVerb(int Index);
        virtual int __fastcall GetVerbCount();


};

//---------------------------------------------------------------------------

#endif
