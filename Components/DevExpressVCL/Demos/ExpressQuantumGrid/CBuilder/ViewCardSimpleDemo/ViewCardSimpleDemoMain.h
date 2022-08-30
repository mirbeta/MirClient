//---------------------------------------------------------------------------

#ifndef ViewCardSimpleDemoMainH
#define ViewCardSimpleDemoMainH
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
#include "cxGridCardView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
//---------------------------------------------------------------------------
class TViewCardSimpleDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TSplitter *Splitter;
  TPanel *pnDepartments;
  TPanel *pnDepartmentsCaption;
  TcxGrid *cxGridDepartments;
  TcxGridDBTableView *cxGridDepartmentsDBTableView;
  TcxGridDBColumn *cxGridDepartmentsDBTableViewNAME;
  TcxGridLevel *cxGridDepartmentsLevel;
  TPanel *pnUsers;
  TcxGrid *cxGridUsers;
  TcxGridDBCardView *cxGridUsersDBCardView;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewFNAME;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewMNAME;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewLNAME;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewCOUNTRY;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewPOSTALCODE;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewCITY;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewADDRESS;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewPHONE;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewFAX;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewEMAIL;
  TcxGridDBCardViewRow *cxGridUsersDBCardViewHOMEPAGE;
  TcxGridLevel *cxGridUsersLevel;
  TPanel *pnUsersCaption;
  TMenuItem *miOptions;
  TMenuItem *miInvertSelect;
  TMenuItem *miCellSelect;
  TMenuItem *miMultiSelect;
  TMenuItem *miHideFocusRect;
  TMenuItem *miShowNavigator;
  void __fastcall miInvertSelectClick(TObject *Sender);
  void __fastcall miCellSelectClick(TObject *Sender);
  void __fastcall miHideFocusRectClick(TObject *Sender);
  void __fastcall miMultiSelectClick(TObject *Sender);
  void __fastcall miShowNavigatorClick(TObject *Sender);
private:  // User declarations
public:   // User declarations
  __fastcall TViewCardSimpleDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewCardSimpleDemoMainForm *ViewCardSimpleDemoMainForm;
//---------------------------------------------------------------------------
#endif
