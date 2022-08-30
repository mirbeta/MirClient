//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoNoteBookH
#define EditorsStylesDemoNoteBookH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include "EditorsStylesDemoBase.h"
#include "cxCalendar.hpp"
#include "cxClasses.hpp"
#include "cxColorComboBox.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBColorComboBox.hpp"
#include "cxDBData.hpp"
#include "cxDBEdit.hpp"
#include "cxDBFontNameComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxFilter.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxImage.hpp"
#include "cxNavigator.hpp"
#include <Graphics.hpp>
#include "cxDBNavigator.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxColorGallery.hpp"
#include "dxDBColorGallery.hpp"
#include "dxGalleryControl.hpp"
//---------------------------------------------------------------------------
class TSpiralImageControl : public TCustomControl {
private:
  Graphics::TBitmap* FBitmap;
protected:
public:
  void __fastcall Paint(void);
	__fastcall virtual TSpiralImageControl(TComponent* AOwner);
	__fastcall virtual ~TSpiralImageControl(void);
  __property Graphics::TBitmap* Bitmap = {read=FBitmap, write=FBitmap};
};

class TEditorsStylesDemoNoteBookFrame : public TEditorsStylesDemoBaseFrame
{
__published:	// IDE-managed Components
  TcxGroupBox *gbDescriptionControls;
  TcxFontNameComboBox *cxFontNameComboBox;
  TcxSpinEdit *cxSpinEdit;
  TcxGroupBox *cxGroupBox2;
  TcxGroupBox *cxGroupBox1;
  TcxGroupBox *gbFont;
  TcxLabel *lbNoteFontColor;
  TcxDBFontNameComboBox *cbFontName;
  TcxDBColorComboBox *cbFontColor;
  TcxLabel *lbTextSize;
  TcxDBSpinEdit *seTextSize;
  TcxLabel *lbBkGround;
  TdxDBColorGallery *dxDBColorGallery1;
  TcxLabel *cxLabel2;
  TcxLabel *lblDate;
  TcxLabel *cxLabel1;
  TcxLabel *lblNote;
  TcxDBDateEdit *cxDBDateEdit1;
  TcxDBTextEdit *cxDBTextEdit1;
  TcxGroupBox *cxGroupBox3;
  TPanel *pnSpiral;
  TcxDBMemo *dbmText;
  TcxDBNavigator *cxDBNavigator1;
  TcxImage* cxImage1;
  void __fastcall tblNoteBookAfterScroll(TDataSet *DataSet);
  void __fastcall dsNoteBookDataChange(TObject *Sender, TField *Field);
  void __fastcall cxFontNameComboBoxPropertiesChange(TObject *Sender);
  void __fastcall cxSpinEditPropertiesChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
  TSpiralImageControl* FSpiralImageControl;
public:		// User declarations
  __fastcall TEditorsStylesDemoNoteBookFrame(TComponent* Owner);
  virtual void ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle);
  bool ShowControlsAboveDescription();
  String __fastcall Name();
  String __fastcall BriefName();
  String StylesIniPath();
  String Description();
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoNoteBookFrame *EditorsStylesDemoNoteBookFrame;
//---------------------------------------------------------------------------
#endif
