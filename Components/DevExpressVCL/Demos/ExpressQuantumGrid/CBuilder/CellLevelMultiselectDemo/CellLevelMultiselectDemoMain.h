//---------------------------------------------------------------------------

#ifndef CellLevelMultiselectDemoMainH
#define CellLevelMultiselectDemoMainH
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
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxLabel.hpp"
#include "cxDataStorage.hpp"
//---------------------------------------------------------------------------
class TCellLevelMultiselectDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TPanel *Panel1;
  TcxLabel *Label1;
  TcxLabel *Label2;
  TcxLabel *Label3;
  TcxLabel *Label4;
  TcxLabel *lblSelectedRows;
  TcxLabel *lblSelectedColumns;
  TcxLabel *lblSelectedCells;
  TcxLabel *lblSelectedSummary;
  TcxGrid *Grid;
  TcxGridTableView *TableView;
  TcxGridLevel *Level;
  TcxStyle *styleSelected;
  TcxStyle *styleNormal;
  void __fastcall TableViewSelectionChanged(TcxCustomGridTableView *Sender);
  void __fastcall TableViewCustomDrawColumnHeader(TcxGridTableView *Sender,
	TcxCanvas *ACanvas, TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone);
  void __fastcall TableViewCustomDrawIndicatorCell(
	TcxGridTableView *Sender, TcxCanvas *ACanvas,
	TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone);
  void __fastcall TableViewMouseDown(TObject *Sender, TMouseButton Button,
  	TShiftState Shift, int X, int Y);
	void __fastcall TableViewStylesGetHeaderStyle(TcxGridTableView *Sender, TcxGridColumn *AColumn,
          TcxStyle *&AStyle);
private:  // User declarations
  static const int RecordCount = 500;
  static const int ColumnCount = 256;
  TcxGridColumn *FAnchorLinkedAreaLastColumn;
  void CreateSpreadSheet();
  void CreateColumns();
  void CreateRows();
  String GetColumnCaption(int Index);
  int GetSummOfSelection();
  void InternalDrawItem(TcxCanvas *ACanvas, TRect ABounds,
    TcxBorders ABorders, String AText, bool AIsSelected);
  int SelectedColumnCount();
  int SelectedRowCount();
  void SetColumnsSelected(TcxGridColumn *AFromColumn, TcxGridColumn *AToColumn,
    bool ASelected);
public:   // User declarations
  __fastcall TCellLevelMultiselectDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCellLevelMultiselectDemoMainForm *CellLevelMultiselectDemoMainForm;
//---------------------------------------------------------------------------
#endif
