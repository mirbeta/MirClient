//---------------------------------------------------------------------------

#ifndef EBarsUtilsH
#define EBarsUtilsH
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
	dxMyDXURL = "https://www.devexpress.com/ClientCenter";
//---------------------------------------------------------------------------
class TdmCommonData : public TDataModule
{
__published:	// IDE-managed Components
        TActionList *alMain;
        TAction *actBarsHelp;
        TAction *actDockingHelp;
        TAction *actRateDemo;
        TAction *actDownloads;
        TAction *actSupport;
        TAction *actMyDX;
		TAction *actDXOnTheWeb;
        TAction *actProducts;
        TAction *actAbout;
        void __fastcall actSBarsHelpExecute(TObject *Sender);
        void __fastcall actBarsHelpExecute(TObject *Sender);
        void __fastcall actDockingHelpExecute(TObject *Sender);
        void __fastcall actRateDemoExecute(TObject *Sender);
        void __fastcall actMyDXExecute(TObject *Sender);
        void __fastcall actDownloadsExecute(TObject *Sender);
        void __fastcall actSupportExecute(TObject *Sender);
        void __fastcall actDXOnTheWebExecute(TObject *Sender);
        void __fastcall actProductsExecute(TObject *Sender);
        void __fastcall actAboutExecute(TObject *Sender);
private:	// User declarations
        TMetaClass* FAboutFormClass;
public:		// User declarations
        __fastcall TdmCommonData(TComponent* Owner);
        __property TMetaClass* AboutFormClass = {read=FAboutFormClass, write=FAboutFormClass, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TdmCommonData *dmCommonData;
//---------------------------------------------------------------------------
void Browse(dxSitePage ASitePage);
//---------------------------------------------------------------------------
#endif