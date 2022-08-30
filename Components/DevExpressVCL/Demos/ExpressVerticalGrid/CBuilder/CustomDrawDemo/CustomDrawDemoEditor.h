//---------------------------------------------------------------------------

#ifndef CustomDrawDemoEditorH
#define CustomDrawDemoEditorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxMRUEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxVGrid.hpp"
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "CustomDrawDemoUtils.h"
#include "CustomDrawDemoMain.h"
//---------------------------------------------------------------------------
class TCustomDrawDemoEditorForm : public TForm
{
__published:	// IDE-managed Components
  TcxButton *btnClose;
  TGroupBox *gbEventHandlerSettings;
  TLabel *lbFont;
  TSpeedButton *sbFont;
  TBevel *bvSeparator;
  TcxComboBox *cbGradient;
  TcxMRUEdit *mruBkImage;
  TcxRadioButton *rbBackGroundImage;
  TcxRadioButton *rbGradient;
  TcxRadioButton *rbDependsOnTheData;
  TcxRadioButton *rbDefaultDrawing;
  TcxCheckBox *chbOwnerDrawText;
  TcxVerticalGrid *vgCustomDrawItems;
  TcxCategoryRow *vgDrawItemCategory;
  TFontDialog *FontDialog;
  TOpenDialog *OpenDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnCloseClick(TObject *Sender);
  void __fastcall mruBkImagePropertiesButtonClick(TObject *Sender);
  void __fastcall mruBkImagePropertiesEditValueChanged(TObject *Sender);
  void __fastcall rbRadioButtonClick(TObject *Sender);
  void __fastcall chbOwnerDrawTextPropertiesChange(TObject *Sender);
  void __fastcall sbFontClick(TObject *Sender);
  void __fastcall cbGradientPropertiesChange(TObject *Sender);
  void __fastcall vgCustomDrawItemsItemChanged(TObject *Sender,
          TcxCustomRow *AOldRow, int AOldCellIndex);
  void __fastcall vgCustomDrawItemsDrawRowHeader(TObject *Sender,
          TcxCanvas *ACanvas, TcxvgPainter *APainter,
          TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done);
  void __fastcall vgCustomDrawItemsStylesGetCategoryStyle(TObject *Sender,
          TcxCustomRow *ARow, TcxStyle *&AStyle);
private:	// User declarations
  void AdjustControlsEnable();
  void AdjustSettings(TcxCategoryRow* ASelectedRow);
  TBkImage GetBkImageTypeByName(String AName);
  void FillCustomDrawItemList();
  void FillBkImageTypeList();
  void FillColorSchemeList();
  TcxItemCustomDrawInfo* GetSelectedDrawItem();
public:		// User declarations
  __fastcall TCustomDrawDemoEditorForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoEditorForm *CustomDrawDemoEditorForm;
//---------------------------------------------------------------------------
#endif
