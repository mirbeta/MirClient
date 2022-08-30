//---------------------------------------------------------------------------

#ifndef EditorsLookupDemoMainH
#define EditorsLookupDemoMainH
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
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxDBEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxTextEdit.hpp"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include "cxDBLookupEdit.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
#include "cxDBNavigator.hpp"
#include "cxPropertiesStore.hpp"
#include "BaseForm.h"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TEditorsLookupDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TPanel *pnlEditors;
	TcxGroupBox *gbIssue;
	TcxLabel *Label1;
	TcxLabel *Label2;
	TcxLabel *Label9;
  TcxDBTextEdit *edName;
  TcxDBMemo *meDescription;
  TcxDBLookupComboBox *lcbCreator;
	TcxGroupBox *gbInfo;
	TcxLabel *Label3;
	TcxLabel *Label4;
  TcxDBImageComboBox *cbPriority;
  TcxDBDateEdit *deCreateDate;
  TcxDBCheckBox *chbRequest;
	TcxGroupBox *gbStatus;
	TcxLabel *Label5;
	TcxLabel *Label6;
	TcxLabel *Label7;
  TcxDBImageComboBox *cbStatus;
  TcxDBDateEdit *deLastModifiedDate;
  TcxDBDateEdit *deFixedDate;
  TcxDBNavigator *DBNavigator;
	TcxGroupBox *gbProject;
	TcxLabel *Label8;
	TcxLabel *Label10;
  TcxDBLookupComboBox *lcbProject;
  TcxDBLookupComboBox *lcbOwner;
  TMenuItem *miOptions;
  TMenuItem *LookupOptions1;
  TMenuItem *miEditMode;
  TMenuItem *miPickMode;
  TMenuItem *miStandardMode;
  TMenuItem *miStorage;
  TMenuItem *miStorageActive;
  TMenuItem *N6;
  TMenuItem *miStorageType;
  TMenuItem *miIniStoreType;
  TMenuItem *miRegistryStoreType;
  TMenuItem *miMemoryStoreType;
  TMenuItem *N5;
  TMenuItem *miStore;
  TMenuItem *miRestore;
  TcxPropertiesStore *cxPropertiesStore;
  TcxPropertiesStore *cxStorageActiveStore;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ChangeLookupModeClick(TObject *Sender);
  void __fastcall lcbCreatorNewLookupDisplayText(TObject *Sender,
	  const TCaption AText);
  void __fastcall StorageTypeClick(TObject *Sender);
  void __fastcall miStoreClick(TObject *Sender);
  void __fastcall miRestoreClick(TObject *Sender);
  void __fastcall miStorageActiveClick(TObject *Sender);
private:  // User declarations
  TMemoryStream *FStream;
  int IndexOfPropertiesStoreComponent(TComponent *AComponent);
  void SetPickLookupMode();
  void SetStandardLookupMode();
  void SetEditLookupMode();
public:   // User declarations
  __fastcall TEditorsLookupDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsLookupDemoMainForm *EditorsLookupDemoMainForm;
//---------------------------------------------------------------------------
#endif
