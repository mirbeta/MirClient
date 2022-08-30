//---------------------------------------------------------------------------

#ifndef StylesMultiDemoMainH
#define StylesMultiDemoMainH
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
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyleSheetEditor.hpp"
#include "cxRadioGroup.hpp"
#include "cxListBox.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "cxContainer.hpp"
#include "cxGroupBox.hpp"
#include "cxLookAndFeels.hpp"
#include "cxGridStyleSheetsPreview.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridCardView.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
//---------------------------------------------------------------------------
class TStylesMultiDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TSplitter *Splitter;
  TcxGrid *cxGrid;
  TcxGridDBTableView *tvProjects;
  TcxGridDBColumn *tvProjectsID;
  TcxGridDBColumn *tvProjectsNAME;
  TcxGridDBColumn *tvProjectsMANAGERID;
  TcxGridDBTableView *tvTeam;
  TcxGridDBColumn *tvTeamPROJECTID;
  TcxGridDBColumn *tvTeamFUNCTION;
  TcxGridDBColumn *tvTeamUSERID;
  TcxGridLevel *lvProjects;
  TcxGridLevel *lvTeam;
  TPanel *pnlLeft;
  TcxGroupBox *gbUserDefined;
  TcxComboBox *cbUserStyleSheets;
  TcxButton *btnLoad;
  TcxButton *btnSave;
  TcxButton *btnEdit;
  TcxRadioGroup *RadioGroup;
  TcxGroupBox *gbPredefined;
  TcxListBox *lbPredefinedStyleSheets;
  TPanel *pnlCurrentStyleSheet;
  TcxGridPopupMenu *cxGridPopupMenu1;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall btnLoadClick(TObject *Sender);
  void __fastcall RadioGroupClick(TObject *Sender);
  void __fastcall cbUserStyleSheetsChange(TObject *Sender);
  void __fastcall lbPredefinedStyleSheetsClick(TObject *Sender);
  void __fastcall btnEditClick(TObject *Sender);
  void __fastcall FormActivate(TObject *Sender);
private:  // User declarations
  TcxGridTableViewStyleSheet* GetCurrentStyleSheet() ;
  void CreateUserStyleSheetsList();
  void CreatePredefinedStyleSheetsList();
  void UpdateGridStyleSheets(TcxGridTableViewStyleSheet *AStyleSheet);
  void UpdateView(TcxGridDBTableView *AView,
    TcxGridTableViewStyleSheet *AStyleSheet);
  void ChangeVisibility(int AType);
  void SetPredefinedStyleSheets();
  void SetUserDefinedStyleSheets();
  void ClearUserDefinedStyleSheets();
  void LoadUserDefinedStyleSheets(TFileName AFileName);
  void SaveUserDefinedStyleSheets(TFileName AFileName);
public:   // User declarations
  __fastcall TStylesMultiDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesMultiDemoMainForm *StylesMultiDemoMainForm;
//---------------------------------------------------------------------------
#endif
