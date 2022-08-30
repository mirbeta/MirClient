//---------------------------------------------------------------------------

#ifndef BandedFixedDemoDataH
#define BandedFixedDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include <DBClient.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TBandedFixedDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TdxMemData *mdSheduler;
  TAutoIncField *mdShedulerID;
  TIntegerField *mdShedulerPROJECTID;
  TIntegerField *mdShedulerProjectManagerID;
  TIntegerField *mdShedulerUSERID;
  TSmallintField *mdShedulerSUNDAY;
  TSmallintField *mdShedulerMONDAY;
  TSmallintField *mdShedulerTUESDAY;
  TSmallintField *mdShedulerWEDNESDAY;
  TSmallintField *mdShedulerTHURSDAY;
  TSmallintField *mdShedulerFRIDAY;
  TSmallintField *mdShedulerSATURDAY;
  TIntegerField *mdShedulerWeekSum;
  TFloatField *mdShedulerWeekAVG;
  TDataSource *dsSheduler;
  TDataSource *dsPersons;
  TdxMemData *mdPersons;
	TAutoIncField *mdPersonsID;
  TStringField *mdPersonsName;
  TStringField *mdPersonsAddress;
  TStringField *mdPersonsPhone;
  TStringField *mdPersonsFax;
  TStringField *mdPersonsEMAIL;
  TcxStyleRepository *StyleRepository;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxStyle *cxStyle8;
  TcxStyle *cxStyle9;
  TcxStyle *cxStyle10;
  TcxStyle *cxStyle11;
  TcxStyle *cxStyle12;
  TcxStyle *cxStyle13;
  TcxStyle *stlGroupNode;
  TcxStyle *stlFixedBand;
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
  TDataSource *dsProjects;
  TdxMemData *mdProjects;
  TAutoIncField *mdProjectsID;
  TStringField *mdProjectsNAME;
  TIntegerField *mdProjectsMANAGERID;
  void __fastcall mdShedulerCalcFields(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
public:
  String GetProjectNameByID(int AProjectID);
  String GetPersonNameByID(int APersonID);
  __fastcall TBandedFixedDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBandedFixedDemoDataDM *BandedFixedDemoDataDM;
//---------------------------------------------------------------------------
#endif
