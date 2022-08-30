//---------------------------------------------------------------------------

#ifndef ColumnsMultiEditorsDemoMainH
#define ColumnsMultiEditorsDemoMainH
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
#include "cxDBEditRepository.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxTLExportLink.hpp"
#include "cxExtEditRepositoryItems.hpp"
// #include "dxSpreadSheetFormattedTextUtils.hpp"
  
#include "cxTLData.hpp"
#include "ColumnsMultiEditorsDemoPopup.h"
#include "cxLookAndFeelPainters.hpp"
#include "cxTLdxBarBuiltInMenu.hpp"
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TColumnsMultiEditorsDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxVirtualTreeList *TreeList;
  TcxTreeListColumn *clnEditorName;
  TcxTreeListColumn *clnSample;
  TcxEditRepository *EditRepository;
  TcxEditRepositoryBlobItem *EditRepositoryBlobItem;
  TcxEditRepositoryButtonItem *EditRepositoryButtonItem;
  TcxEditRepositoryCalcItem *EditRepositoryCalcItem;
  TcxEditRepositoryCheckBoxItem *EditRepositoryCheckBoxItem;
  TcxEditRepositoryComboBoxItem *EditRepositoryComboBoxItem;
  TcxEditRepositoryCurrencyItem *EditRepositoryCurrencyItem;
  TcxEditRepositoryDateItem *EditRepositoryDateItem;
  TcxEditRepositoryHyperLinkItem *EditRepositoryHyperLinkItem;
  TcxEditRepositoryImageItem *EditRepositoryImageItem;
  TcxEditRepositoryImageComboBoxItem *EditRepositoryImageComboBoxItem;
  TcxEditRepositoryLookupComboBoxItem *EditRepositoryLookupComboBoxItem;
  TcxEditRepositoryMaskItem *EditRepositoryMaskItem;
  TcxEditRepositoryRichItem *EditRepositoryMemoItem;
  TcxEditRepositoryMRUItem *EditRepositoryMRUItem;
  TcxEditRepositoryPopupItem *EditRepositoryPopupItem;
  TcxEditRepositoryRadioGroupItem *EditRepositoryRadioGroupItem;
  TcxEditRepositorySpinItem *EditRepositorySpinItem;
  TcxEditRepositoryTextItem *EditRepositoryTextItem;
  TcxEditRepositoryTimeItem *EditRepositoryTimeItem;
  TMenuItem *Export1;
  TMenuItem *miExportToXLS;
  TMenuItem *miExporttoXLSX;
  TMenuItem *miExporttoHTML;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall clnSampleGetEditProperties(TcxTreeListColumn *Sender,
    TcxTreeListNode *ANode, TcxCustomEditProperties *&EditProperties);
  void __fastcall EditRepositoryButtonItemPropertiesButtonClick(
    TObject *Sender, int AButtonIndex);
  void __fastcall EditRepositoryPopupItemPropertiesInitPopup(TObject *Sender);
  void __fastcall TreeListStylesGetContentStyle(TcxCustomTreeList *Sender, 
    TcxTreeListColumn *AColumn, TcxTreeListNode *ANode, TcxStyle *&AStyle);
  void __fastcall TreeListStylesGetNodeIndentStyle(TcxCustomTreeList *Sender,
    TcxTreeListNode *ANode, int ALevel, TcxStyle *&AStyle);
  void __fastcall miExporttoHTMLClick(TObject *Sender);
  void __fastcall ExportToXLS1Click(TObject *Sender);
  void __fastcall miExporttoXLSXClick(TObject *Sender);
private:
    TColumnsMultiEditorsDemoPopupForm *FPopupForm;
protected:
  int GetNodeItemIndex(TcxTreeListNode *ANode);
  int RootCount();
  void AfterExport(AnsiString AFileName);
public:		// User declarations
  __fastcall TColumnsMultiEditorsDemoMainForm(TComponent* Owner);
};

enum TcxEditorsCategoryType { ectStandard, ectComboBoxes, ectBlobs, ectPopups };
const int cxEditorsCategoryCount = 4;
const int cxEditorsTypeCount = 19;
enum TcxEditorsType { etBlob, etButton, etCalc, etCheckBox, etComboBox, etCurrency,
  etDate, etHyperLink, etImage, etImageComboBox, etLookupComboBox, etMask,
  etMemo, etMRU, etPopup, etRadioGroup, etSpinItem, etText, etTime };
const String EditorsCategoryNames[cxEditorsCategoryCount] =
    {"Standard Editors", "ComboBoxes", "Blobs", "Popups"};

class TColumnsMultiEditorsDemoDataSource : public TcxTreeListCustomDataSource
{
private:
  TcxEditRepository *FEditRepository;
  Variant FValues[cxEditorsTypeCount];
  String FEditorNames[cxEditorsTypeCount];
  TcxEditorsCategoryType FCategories[cxEditorsTypeCount];
  TcxVirtualTreeList *FTreeList;
  String GetEditorName(int AEditorIndex);
  Variant GetEditorValue(int AEditorIndex);
  void FillEditorsNames();
  void FillEditorsValues();
  void FillEditorsCategories();
  void SetEditorValue(int AEditorIndex, const Variant AValue);
protected:
	virtual int __fastcall GetRecordCount(void);
	virtual Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
	virtual void __fastcall SetValue(void * ARecordHandle, void * AItemHandle,
      const Variant &AValue);
	virtual void* __fastcall GetParentRecordHandle(void *ARecordHandle);
public:
  __fastcall TColumnsMultiEditorsDemoDataSource(TcxVirtualTreeList *ATreeList,
    TcxEditRepository *AEditRepository);
  int RootCount(void);
};

//---------------------------------------------------------------------------
extern PACKAGE TColumnsMultiEditorsDemoMainForm *ColumnsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------
#endif
