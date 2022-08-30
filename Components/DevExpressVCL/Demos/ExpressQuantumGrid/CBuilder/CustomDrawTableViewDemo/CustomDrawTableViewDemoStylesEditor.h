//---------------------------------------------------------------------------

#ifndef CustomDrawTableViewDemoStylesEditorH
#define CustomDrawTableViewDemoStylesEditorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxMRUEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxRadioGroup.hpp"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "CustomDrawTableViewDemoTypes.h"
#include "cxImageComboBox.hpp"
//---------------------------------------------------------------------------
class TCustomDrawTableViewDemoStylesEditorForm : public TForm
{
__published:	// IDE-managed Components
  TcxButton *btnClose;
  TTreeView *tvCustomDrawItems;
  TGroupBox *gbEventHandlerSettings;
  TLabel *lbFont;
  TSpeedButton *sbFont;
  TBevel *bvSeparator;
  TLabel *lbIndicatorGlyph;
  TcxComboBox *cbGradient;
  TcxMRUEdit *mruBkImage;
  TcxRadioButton *rbBackGroundImage;
  TcxRadioButton *rbGradient;
  TcxRadioButton *rbDependsOnTheData;
  TcxRadioButton *rbDafaultDrawing;
  TPanel *pnSampleText;
  TcxImageComboBox *icbIndicatorImages;
  TFontDialog *FontDialog;
  TOpenDialog *OpenDialog;
  void __fastcall tvCustomDrawItemsClick(TObject *Sender);
  void __fastcall sbFontClick(TObject *Sender);
  void __fastcall cbGradientPropertiesChange(TObject *Sender);
  void __fastcall rbBackGroundImageClick(TObject *Sender);
  void __fastcall rbGradientClick(TObject *Sender);
  void __fastcall rbDafaultDrawingClick(TObject *Sender);
  void __fastcall rbDependsOnTheDataClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall mruBkImagePropertiesChange(TObject *Sender);
  void __fastcall mruBkImagePropertiesButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall mruBkImageKeyPress(TObject *Sender, char &Key);
  void __fastcall btnCloseClick(TObject *Sender);
  void __fastcall icbIndicatorImagesPropertiesEditValueChanged(
          TObject *Sender);
private:	// User declarations
  Graphics::TBitmap* FUserDefinedImage;
  bool FIsUpdating;
  void __fastcall SetFontVisibility(bool IsVisible = false);
  void __fastcall SetDependsOnTheDataVisibility(bool IsVisible = false);
  void __fastcall SetIndicatorCellVisibility(bool AIsVisible);
  void __fastcall SetProperties(TCustomDrawingStyle ACustomDrawingStyle);
  void __fastcall SetIndicatorImage(TViewType AViewType);
  String __fastcall GetBkImageText();
  String __fastcall GetGradientColorText();
  void __fastcall DisableControls(TcxCustomMaskEdit* Sender);
public:		// User declarations
  __fastcall TCustomDrawTableViewDemoStylesEditorForm(TComponent* Owner);
  void __fastcall SetFont();
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawTableViewDemoStylesEditorForm *CustomDrawTableViewDemoStylesEditorForm;
//---------------------------------------------------------------------------
#endif
