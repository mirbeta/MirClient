//---------------------------------------------------------------------------
#ifndef SplashH
#define SplashH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TfmSplash : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblText;
  TBevel *Bevel1;
  TPanel *pnlImageHost;
  TImage *Image1;
  TButton *btnOK;
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfmSplash(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
