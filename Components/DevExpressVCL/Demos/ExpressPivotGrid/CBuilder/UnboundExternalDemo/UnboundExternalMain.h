//---------------------------------------------------------------------------

#ifndef UnboundExternalMainH
#define UnboundExternalMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxPivotGrid.hpp"
//---------------------------------------------------------------------------

class  TcxExternalDataSource : public TcxCustomDataSource
{
private:
  int FFieldCount;
  String GetFieldName(int AIndex);
  String GetFieldType(int AIndex);
protected:
  TStringList* Records;
  void __fastcall AfterLoad();
  int  __fastcall GetRecordCount();
  String __fastcall GetItemFromStr(const String AString);
  Variant __fastcall GetValue(void * ARecordHandle, void * AItemHandle);
  Variant __fastcall GetValueFromString(String ARecord, int AIndex);
  int __fastcall GetVarTypeByName(const String AName);
public:
  __fastcall TcxExternalDataSource(const String FileName);
  __fastcall ~TcxExternalDataSource(void);

  __property int FieldCount = {read = FFieldCount};
  __property String FieldNames[int AIndex] = {read = GetFieldName};
  __property String FieldTypes[int AIndex] = {read = GetFieldType};
  __property int RecordCount = {read = GetRecordCount};
};

class TfrmUnboundExternal : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
        TcxPivotGrid *UnboundPivotGrid;
    void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
    TcxExternalDataSource* ExternalData;
    TcxCustomPivotGrid* __fastcall PivotGrid();
    void __fastcall SetFieldPos(const String AFieldName, TcxPivotGridFieldArea AArea);
public:		// User declarations
    __fastcall TfrmUnboundExternal(TComponent* Owner);
};


//---------------------------------------------------------------------------
extern PACKAGE TfrmUnboundExternal *frmUnboundExternal;
//---------------------------------------------------------------------------
#endif
