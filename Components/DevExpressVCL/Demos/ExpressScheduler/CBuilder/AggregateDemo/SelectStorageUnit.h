//---------------------------------------------------------------------------

#ifndef SelectStorageUnitH
#define SelectStorageUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TSelectStorage : public TForm
{
__published:	// IDE-managed Components
		TGroupBox *GroupBox1;
		TRadioButton *rbDBStorage;
		TRadioButton *rbUnboundStorage;
		TButton *Button1;
		TButton *Button2;
private:	// User declarations
public:		// User declarations
	__fastcall TSelectStorage(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSelectStorage *SelectStorage;
//---------------------------------------------------------------------------
#endif
