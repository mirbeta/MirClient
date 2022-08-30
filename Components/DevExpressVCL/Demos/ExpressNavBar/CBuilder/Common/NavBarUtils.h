//---------------------------------------------------------------------------

#ifndef NavBarUtilsH
#define NavBarUtilsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
enum dxSitePage { spDownloads, spSupport, spStart, spProducts, spMyDX };
//---------------------------------------------------------------------------
const
  PCHAR
	dxDownloadURL = "http://www.devexpress.com/downloads",
	dxSupportURL = "http://www.devexpress.com/Support/Center",
	dxStartURL = "http://www.devexpress.com",
	dxProductsURL = "http://www.devexpress.com/products",
  	dxMyDXURL = "http://www.mydevexpress.com";
//---------------------------------------------------------------------------
class TdmCommonData : public TDataModule
{
__published:	// IDE-managed Components
        TActionList *alMain;
        TAction *actDownloads;
        TAction *actSupport;
        TAction *actDXOnTheWeb;
        TAction *actProducts;
        TAction *actExit;
        TImageList *ilMain;
        void __fastcall actExitExecute(TObject *Sender);
        void __fastcall actDownloadsExecute(TObject *Sender);
        void __fastcall actSupportExecute(TObject *Sender);
        void __fastcall actDXOnTheWebExecute(TObject *Sender);
        void __fastcall actProductsExecute(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TdmCommonData(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmCommonData *dmCommonData;
//---------------------------------------------------------------------------
void Browse(dxSitePage ASitePage);
//---------------------------------------------------------------------------
#endif