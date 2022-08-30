//---------------------------------------------------------------------------

#ifndef CustomRowHeightDemoMainH
#define CustomRowHeightDemoMainH
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
#include "cxDataStorage.hpp"
#include "cxImage.hpp"
#include "cxMemo.hpp"
//---------------------------------------------------------------------------
class TCustomRowHeightDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBTableView *tvFilms;
  TcxGridDBColumn *tvFilmsCAPTION;
  TcxGridDBColumn *tvFilmsPLOTOUTLINE;
  TcxGridDBColumn *tvFilmsPHOTO;
  TcxGridLevel *lvFilms;
  TMenuItem *miOptions;
  TMenuItem *miPictureZoom;
  TMenuItem *miZoom100perc;
  TMenuItem *miZoom75perc;
  TMenuItem *miZoom50perc;
  TMenuItem *miZoom25perc;
  void __fastcall miZoomClick(TObject *Sender);
  void __fastcall tvFilmsGetCellHeight(TcxCustomGridTableView *Sender,
      TcxCustomGridRecord *ARecord, TcxCustomGridTableItem *AItem,
      TcxGridTableDataCellViewInfo *ACellViewInfo, int &AHeight);
private:  // User declarations
  int FCurrentZoom;
  int GetZoomByMenuItem(int AMenuItemIndex);
public:   // User declarations
  __fastcall TCustomRowHeightDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomRowHeightDemoMainForm *CustomRowHeightDemoMainForm;
//---------------------------------------------------------------------------
#endif
