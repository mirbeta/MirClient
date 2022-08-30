//---------------------------------------------------------------------------

#ifndef DockingMegaDemoH
#define DockingMegaDemoH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxBar.hpp"
#include "dxDockControl.hpp"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfmMain : public TForm
{
__published:	// IDE-managed Components
  TActionList *alMain;
  TAction *actHelp;
  TAction *actRateDemo;
  TAction *actDownloads;
  TAction *actForum;
  TAction *actDXOnTheWeb;
  TAction *actProducts;
  TAction *actAbout;
  TAction *actShowDemoDescription;
  TdxDockingManager *dxDockingManager1;
  TdxBarManager *BarManager;
  TdxBarButton *dxBarButtonLoad;
  TdxBarButton *dxBarButtonSave;
  TdxBarSubItem *dxBarSubItemFile;
  TdxBarSubItem *dxBarSubItemInsert;
  TdxBarSubItem *dxBarSubItemFormat;
  TdxBarSubItem *dxBarSubItemWindow;
  TdxBarButton *dxBarButtonExit;
  TdxBarSubItem *dxBarSubItemHelp;
  TdxBarButton *dxBarButtonStartPage;
  TdxBarButton *dxBarButtonToolBox;
  TdxBarButton *dxBarButtonWatch;
  TdxBarButton *dxBarButtonOutput;
  TdxBarButton *dxBarButtonCallStack;
  TdxBarButton *dxBarButtonProperties;
  TdxBarButton *dxBarButtonClassView;
  TdxBarButton *dxBarButtonFont;
  TdxBarButton *dxBarButtonColor;
  TdxBarButton *dxBarButtonStandardView;
  TdxBarButton *dxBarButtonNETView;
  TdxBarButton *dxBarButtonOffice11View;
  TdxBarButton *dxBarButtonSolutionExplorer;
  TdxBarButton *dxBarButtonDockable;
  TdxBarButton *dxBarButtonHide;
  TdxBarButton *dxBarButtonFloating;
  TdxBarButton *dxBarButtonAutoHide;
  TdxBarSubItem *dxBarSubItemOtherWindows;
  TdxBarButton *dxBarButtonXPView;
  TdxBarButton *dxBarButton1;
  TdxBarButton *dxBarButton2;
  TdxBarButton *dxBarButton3;
  TdxBarButton *dxBarButton4;
  TdxBarButton *dxBarButton5;
  TdxBarButton *dxBarButton6;
  TdxBarButton *dxBarButton7;
  TdxBarPopupMenu *dxBarPopupMenu1;
  TFontDialog *FontDialog1;
  TColorDialog *ColorDialog1;
  TOpenDialog *OpenDialog1;
  TSaveDialog *SaveDialog1;
  TImageList *ilDockIcons;
  TImageList *imBarIcons;
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmMain *fmMain;
//---------------------------------------------------------------------------
#endif
 