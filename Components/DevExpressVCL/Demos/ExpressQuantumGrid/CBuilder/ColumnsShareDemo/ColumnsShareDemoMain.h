//---------------------------------------------------------------------------

#ifndef ColumnsShareDemoMainH
#define ColumnsShareDemoMainH
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
#include "cxGridCardView.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridDBCardView.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxDBEditRepository.hpp"
#include "cxDBExtLookupComboBox.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
//---------------------------------------------------------------------------
class TColumnsShareDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TcxGrid *Grid;
  TcxGridDBTableView *tvProjects;
  TcxGridDBColumn *tvProjectsNAME;
  TcxGridDBColumn *tvProjectsMANAGERID;
  TcxGridDBTableView *tvItems;
  TcxGridDBColumn *tvItemsNAME;
  TcxGridDBColumn *tvItemsTYPE;
  TcxGridDBColumn *tvItemsPROJECTID;
  TcxGridDBColumn *tvItemsPRIORITY;
  TcxGridDBColumn *tvItemsSTATUS;
  TcxGridDBColumn *tvItemsCREATORID;
  TcxGridDBColumn *tvItemsCREATEDDATE;
  TcxGridDBColumn *tvItemsOWNERID;
  TcxGridDBColumn *tvItemsLASTMODIFIEDDATE;
  TcxGridDBColumn *tvItemsFIXEDDATE;
  TcxGridDBColumn *tvItemsDESCRIPTION;
  TcxGridDBCardView *cvUsers;
  TcxGridDBCardViewRow *cvUsersFNAME;
  TcxGridDBCardViewRow *cvUsersMNAME;
  TcxGridDBCardViewRow *cvUsersLNAME;
  TcxGridDBCardViewRow *cvUsersDepartment;
  TcxGridDBCardViewRow *cvUsersCOUNTRY;
  TcxGridDBCardViewRow *cvUsersPOSTALCODE;
  TcxGridDBCardViewRow *cvUsersCITY;
  TcxGridDBCardViewRow *cvUsersADDRESS;
  TcxGridDBCardViewRow *cvUsersPHONE;
  TcxGridDBCardViewRow *cvUsersFAX;
  TcxGridDBCardViewRow *cvUsersEMAIL;
  TcxGridDBCardViewRow *cvUsersHOMEPAGE;
  TcxGridDBTableView *tvTeam;
  TcxGridDBColumn *tvTeamUSERID;
  TcxGridDBColumn *tvTeamFUNCTION;
  TcxGridLevel *lvItems;
  TcxGridLevel *lvProjects;
  TcxGridLevel *lvTeam;
  TcxGridLevel *lvUsers;
  TcxGridPopupMenu *cxGridPopupMenu1;
  TcxGridViewRepository *cxGridViewRepository;
  TcxGridDBTableView *cxGridViewRepositoryDBTableView;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewID;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewUserName;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewDepartment;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewCOUNTRY;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewPOSTALCODE;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewCITY;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewADDRESS;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewPHONE;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewFAX;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewEMAIL;
  TcxGridDBColumn *cxGridViewRepositoryDBTableViewHOMEPAGE;
  TcxEditRepository *cxEditRepository;
  TcxEditRepositoryExtLookupComboBoxItem *erExtLookupComboBoxItem;
  TcxEditRepositoryLookupComboBoxItem *eriLookupComboUsers;
  TMenuItem *miOptions;
  TMenuItem *miPersonEditor;
  TMenuItem *miLookupEditor;
  TMenuItem *miExtLookUpEditor;
  TMenuItem *miCustomizePersonsLookupCombobox;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miCustomizePersonsLookupComboboxClick(TObject *Sender);
  void __fastcall miLookUpEditorClick(TObject *Sender);
  void __fastcall miExtLookUpEditorClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TColumnsShareDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsShareDemoMainForm *ColumnsShareDemoMainForm;
//---------------------------------------------------------------------------
#endif
