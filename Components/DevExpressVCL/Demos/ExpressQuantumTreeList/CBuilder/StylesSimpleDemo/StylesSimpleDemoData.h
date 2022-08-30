//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoDataH
#define StylesSimpleDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TStylesSimpleDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsDEPARTMENTS;
  TDataSource *dsPERSONS;
  TcxStyleRepository *StyleRepository;
  TcxStyle *Sunny;
  TcxStyle *Dark;
  TcxStyle *Golden;
  TcxStyle *Summer;
  TcxStyle *Autumn;
  TcxStyle *Bright;
  TcxStyle *Cold;
  TcxStyle *Spring;
  TcxStyle *Light;
  TcxStyle *Winter;
  TcxStyle *Depth;
  TcxTreeListStyleSheet *UserStyleSheet;
	TdxMemData *mdDEPARTMENTS;
	TAutoIncField *mdDEPARTMENTSID;
	TIntegerField *mdDEPARTMENTSPARENTID;
	TIntegerField *mdDEPARTMENTSMANAGERID;
	TStringField *mdDEPARTMENTSNAME;
	TFloatField *mdDEPARTMENTSBUDGET;
	TStringField *mdDEPARTMENTSLOCATION;
	TStringField *mdDEPARTMENTSPHONE;
	TStringField *mdDEPARTMENTSFAX;
	TStringField *mdDEPARTMENTSEMAIL;
	TBooleanField *mdDEPARTMENTSVACANCY;
	TdxMemData *mdPERSONS;
	TAutoIncField *mdPERSONSID;
	TStringField *mdPERSONSName;
	TStringField *mdPERSONSCountry;
	TStringField *mdPERSONSPostalCode;
	TStringField *mdPERSONSCity;
	TStringField *mdPERSONSAddress;
	TStringField *mdPERSONSPhone;
	TStringField *mdPERSONSFax;
	TStringField *mdPERSONSEMAIL;
	TStringField *mdPERSONSHOMEPAGE;
	TIntegerField *mdPERSONSDepartmentID;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  void __fastcall SetParentValue(Variant AValue);
  __fastcall TStylesSimpleDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoDataDM *StylesSimpleDemoDataDM;
//---------------------------------------------------------------------------
#endif
