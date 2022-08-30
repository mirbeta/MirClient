//---------------------------------------------------------------------------

#ifndef SimpleVerticalGridDemoMainH
#define SimpleVerticalGridDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxDBVGrid.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxVGrid.hpp"
//---------------------------------------------------------------------------
class TSimpleVerticalGridDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxDBMultiEditorRow *cxDBVerticalGridID;
  TcxDBEditorRow *fldTrademark;
  TcxDBEditorRow *fldModel;
  TcxDBEditorRow *fldCategory;
  TcxCategoryRow *rowPerformance_Attributes;
  TcxDBEditorRow *fldHP;
  TcxDBEditorRow *fldLiter;
  TcxDBEditorRow *fldCyl;
  TcxDBEditorRow *fldTransmissSpeedCount;
  TcxDBEditorRow *fldTransmissAutomatic;
  TcxDBMultiEditorRow *cxDBVerticalGrid1DBMultiEditorRow1;
  TcxCategoryRow *rowNotes;
  TcxDBEditorRow *fldDescription;
  TcxDBEditorRow *fldHyperlink;
  TcxCategoryRow *rowOthers;
  TcxDBEditorRow *fldPrice;
  TcxDBEditorRow *fldPicture;
  TcxEditRepository *cxEditRepository1;
  TcxEditRepositoryImageComboBoxItem *cxEditRepository1ImageComboBoxItem1;
  TcxEditRepositoryImageComboBoxItem *cxEditRepository1ImageComboBoxItem2;
  TcxEditRepositoryCalcItem *cxEditRepository1CalcItem1;
  TImageList *ImageList;
  void __fastcall  actBandSizingExecute(TObject *Sender);
  void __fastcall  actCellHintsExecute(TObject *Sender);
  void __fastcall  actRowSizingExecute(TObject *Sender);
  void __fastcall  actImmediateEditorExecute(TObject *Sender);
  void __fastcall  actPaintStyleExecute(TObject *Sender);
  void __fastcall  cxDBVerticalGridStylesGetContentStyle(TObject *Sender,
    TcxCustomEditorRowProperties *AEditProp, bool AFocused,
    int ARecordIndex, TcxStyle *&AStyle);
  void __fastcall  LayOutStyleExecute(TObject *Sender);
  void __fastcall  miExplorerStyleCategoryClick(TObject *Sender);
  void __fastcall  miGridLinesClick(TObject *Sender);
  void __fastcall  miHeadersClick(TObject *Sender);
  void __fastcall  miIncSearchClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSimpleVerticalGridDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSimpleVerticalGridDemoMainForm *SimpleVerticalGridDemoMainForm;
//---------------------------------------------------------------------------
#endif
