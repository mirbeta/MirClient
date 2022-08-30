//---------------------------------------------------------------------------

#ifndef UnboundExternalDataDemoMainH
#define UnboundExternalDataDemoMainH
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
#include <Dialogs.hpp>
#include "UnboundExternalDataDemoClasses.h"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TUnboundExternalDataDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridTableView *tvSections;
  TcxGridTableView *tvValues;
  TcxGridLevel *SectionLevel;
  TcxGridLevel *DetailLevel;
  TMenuItem *miOpen;
  TMenuItem *miSave;
  TMenuItem *miSaveAs;
  TMenuItem *N1;
  TMenuItem *Edit1;
  TMenuItem *miInsertSection;
  TMenuItem *miDeleteSection;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  void __fastcall miInsertSectionClick(TObject *Sender);
  void __fastcall miDeleteSectionClick(TObject *Sender);
  void __fastcall miOpenClick(TObject *Sender);
  void __fastcall miSaveClick(TObject *Sender);
  void __fastcall miSaveAsClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall sbMainResize(TObject *Sender);
private:  // User declarations
  int FChangesCount;
  TUserIniFile* IniFile;
  TUserDataSource* UserDataSource;
  TUserDetailDataSource* UserDetailDataSource;
  void __fastcall CustomizeGrid();
  void __fastcall GenerateColumns();
  void __fastcall LoadData();
  void __fastcall Load(String const AFileName);
  void __fastcall UpdateFileInfo(String const AFileName);
  void __fastcall ResetChanges();
  void __fastcall DoSmthOnModify(TObject* Sender);
public:   // User declarations
  __fastcall TUnboundExternalDataDemoMainForm(TComponent* Owner);
  virtual void __fastcall AfterConstruction(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundExternalDataDemoMainForm *UnboundExternalDataDemoMainForm;
//---------------------------------------------------------------------------
#endif
