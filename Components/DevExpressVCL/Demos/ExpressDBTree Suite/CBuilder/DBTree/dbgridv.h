//---------------------------------------------------------------------------
#ifndef dbgridvH
#define dbgridvH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <DBCtrls.hpp>
#include <Buttons.hpp>
#include <DBGrids.hpp>
#include "Grids.hpp"
//---------------------------------------------------------------------------
class TForm3 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TDBNavigator *DBNavigator1;
	TBitBtn *BitBtn1;
	TPanel *Panel2;
	TDBGrid *DBGrid1;
private:	// User declarations
public:		// User declarations
	__fastcall TForm3(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm3 *Form3;
//---------------------------------------------------------------------------
#endif
