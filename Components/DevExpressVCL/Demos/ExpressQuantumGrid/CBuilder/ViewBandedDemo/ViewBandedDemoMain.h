//---------------------------------------------------------------------------

#ifndef ViewBandedDemoMainH
#define ViewBandedDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include "cxListBox.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TViewBandedDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridDBBandedTableView *btvItems;
  TcxGridDBBandedColumn *btvItemsNAME;
  TcxGridDBBandedColumn *btvItemsTYPE;
  TcxGridDBBandedColumn *btvItemsPRIORITY;
  TcxGridDBBandedColumn *btvItemsPROJECTID;
  TcxGridDBBandedColumn *btvItemsSTATUS;
  TcxGridDBBandedColumn *btvItemsDESCRIPTION;
  TcxGridDBBandedColumn *btvItemsFIXEDDATE;
  TcxGridDBBandedColumn *btvItemsCREATEDDATE;
  TcxGridDBBandedColumn *btvItemsCreatorName;
  TcxGridDBBandedColumn *btvItemsCreatorPhone;
  TcxGridDBBandedColumn *btvItemsOwnerName;
  TcxGridDBBandedColumn *btvItemsOwnerPhone;
  TcxGridLevel *glItems;
  TMenuItem *miOptions;
  TMenuItem *miBandSizing;
  TMenuItem *miBandMoving;
  TMenuItem *miCreateBand;
  TMenuItem *miDeleteBand;
  TMenuItem *N1;
  TMenuItem *miShowPreviewRow;
  TMenuItem *miColumnsCustomization;
  TImageList *ilMain;
  TMenuItem *miShowNavigator;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miCreateBandClick(TObject *Sender);
  void __fastcall miDeleteBandClick(TObject *Sender);
  void __fastcall miColumnsCustomizationClick(TObject *Sender);
  void __fastcall miShowPreviewRowClick(TObject *Sender);
  void __fastcall miBandMovingClick(TObject *Sender);
  void __fastcall miBandSizingClick(TObject *Sender);
  void __fastcall miShowNavigatorClick(TObject *Sender);
private:	// User declarations
  void __fastcall RemoveBands(TcxListBox* AListBox);
  void __fastcall AddBands(TStrings* AStringList);
  void __fastcall HideCoulmns();
  TcxGridBand* __fastcall GetBandByCaption(String ABandCaption);
  void __fastcall DeleteBand(String ABandCaption);
public:		// User declarations
  __fastcall TViewBandedDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewBandedDemoMainForm *ViewBandedDemoMainForm;
//---------------------------------------------------------------------------
#endif
