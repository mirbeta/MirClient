//---------------------------------------------------------------------------

#ifndef BackgroundDemoMainH
#define BackgroundDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxBar.hpp"
#include "dxBarExtItems.hpp"
#include <ActnList.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <ImgList.hpp>
#include "EBarsUtils.h"
#include "cxClasses.hpp"
//---------------------------------------------------------------------------
class TBackgroundDemoMainForm : public TForm
{
__published:	// IDE-managed Components
  TRadioGroup *BarManagerStyle;
  TGroupBox *gbBackgroundOptions;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TSpeedButton *sbBarBackgroud;
  TSpeedButton *sbBarSubmenu;
  TSpeedButton *sbSatusbarBackgroud;
  TSpeedButton *sbPopupmenu;
  TLabel *lbBarBackgroud;
  TLabel *lbBarSubmenu;
  TLabel *lbSatusbarBackgroud;
  TLabel *lbPopupmenu;
  TBevel *Bevel1;
  TLabel *Label4;
  TCheckBox *cbBar;
  TCheckBox *cbSubMenu;
  TCheckBox *cbBackgroundBitmap;
  TCheckBox *cbBackgroundBitmapPM;
  TButton *btnRestoreDefaults;
  TGroupBox *gbImageDisplay;
  TCheckBox *cbShowImageCaptions;
  TCheckBox *cbAssignHotImages;
  TPanel *Panel1;
  TPanel *pnlMain;
  TPanel *pnlOptions;
  TImageList *ilLargeImages;
  TImageList *ilDisabledImages;
  TImageList *ilHotImages;
  TdxBarManager *dxBarManager1;
  TdxBarSubItem *dxBarSubItem1;
  TdxBarSubItem *dxBarSubItem2;
  TdxBarSubItem *dxBarSubItem3;
  TdxBarSubItem *dxBarSubItem4;
  TdxBarSubItem *dxBarSubItem5;
  TdxBarSubItem *dxBarSubItem6;
  TdxBarLargeButton *dxBarButton1;
  TdxBarLargeButton *dxBarButton3;
  TdxBarLargeButton *dxBarButton4;
  TdxBarLargeButton *dxBarButton5;
  TdxBarLargeButton *dxBarButton6;
  TdxBarLargeButton *dxBarButton7;
  TdxBarLargeButton *dxBarButton8;
  TdxBarLargeButton *dxBarButton9;
  TdxBarLargeButton *dxBarButton10;
  TdxBarLargeButton *dxBarButton11;
  TdxBarLargeButton *dxBarLargeButton1;
  TdxBarLargeButton *dxBarLargeButton2;
  TdxBarLargeButton *dxBarLargeButton3;
  TdxBarLargeButton *dxBarLargeButton4;
  TdxBarLargeButton *dxBarLargeButton5;
  TdxBarLargeButton *dxBarLargeButton6;
  TdxBarLargeButton *dxBarLargeButton7;
  TdxBarButton *dxBarButton15;
  TdxBarColorCombo *dxBarColorCombo1;
  TdxBarFontNameCombo *dxBarFontNameCombo1;
  TdxBarDateCombo *dxBarDateCombo1;
  TdxBarSpinEdit *dxBarSpinEdit1;
  TdxBarProgressItem *dxBarProgressItem1;
  TdxBarStatic *dxBarStatic1;
  TdxBarLargeButton *dxBarLargeButton8;
  TdxBarLargeButton *dxBarLargeButton9;
  TdxBarLargeButton *dxBarLargeButton10;
  TdxBarLargeButton *dxBarLargeButton11;
  TdxBarLargeButton *dxBarLargeButton12;
  TdxBarLargeButton *dxBarLargeButton13;
  TdxBarLargeButton *dxBarLargeButton14;
  TdxBarLargeButton *btnNewWindow;
  TdxBarLargeButton *btnArrangeAll;
  TdxBarLargeButton *btnSplit;
  TdxBarLargeButton *btnNew;
  TdxBarLargeButton *btnOpen;
  TdxBarLargeButton *btnSave;
  TdxBarLargeButton *btnClose;
  TdxBarLargeButton *btnPrint;
  TdxBarLargeButton *btnSaveAs;
  TdxBarLargeButton *btnSaveAll;
  TdxBarSubItem *dxBarSubItem7;
  TdxBarLargeButton *dxBarLargeButton15;
  TdxBarLargeButton *dxBarLargeButton16;
  TdxBarPopupMenu *dxBarPopupMenu1;
  TOpenPictureDialog *OpenPictureDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall cbBarClick(TObject *Sender);
  void __fastcall cbSubMenuClick(TObject *Sender);
  void __fastcall cbBackgroundBitmapClick(TObject *Sender);
  void __fastcall cbBackgroundBitmapPMClick(TObject *Sender);
  void __fastcall btnRestoreDefaultsClick(TObject *Sender);
  void __fastcall cbShowImageCaptionsClick(TObject *Sender);
  void __fastcall cbAssignHotImagesClick(TObject *Sender);
  void __fastcall BarManagerStyleClick(TObject *Sender);
  void __fastcall sbBarBackgroudClick(TObject *Sender);
  void __fastcall dxBarButton1Click(TObject *Sender);
  void __fastcall dxBarSpinEdit1CurChange(TObject *Sender);
private:	// User declarations
  Graphics::TBitmap* FBarBackgroudBitmap, *FBarSubmenuBitmap, *FSatusbarBackgroudBitmap, *FPopupmenuBitmap;
  String FPath;
public:		// User declarations
  __fastcall TBackgroundDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBackgroundDemoMainForm *BackgroundDemoMainForm;
//---------------------------------------------------------------------------
#endif
