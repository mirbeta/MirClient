//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoDataH
#define StylesSimpleDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <Db.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TStylesSimpleDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsPersons;
  TDataSource *dsCountries;
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
        TcxGridTableViewStyleSheet *UserStyleSheet;
	TClientDataSet *cdsPersons;
	TAutoIncField *cdsPersonsID;
	TStringField *cdsPersonsFIRSTNAME;
	TStringField *cdsPersonsSECONDNAME;
	TBooleanField *cdsPersonsGENDER;
	TStringField *cdsPersonsBIRTHNAME;
	TDateTimeField *cdsPersonsDATEOFBIRTH;
	TIntegerField *cdsPersonsBIRTHCOUNTRY;
	TStringField *cdsPersonsLOCATIONOFBIRTH;
	TMemoField *cdsPersonsBIOGRAPHY;
	TStringField *cdsPersonsNICKNAME;
	TStringField *cdsPersonsHOMEPAGE;
	TClientDataSet *cdsCountries;
	TAutoIncField *cdsCountriesID;
	TStringField *cdsCountriesNAME;
	TStringField *cdsCountriesACRONYM;
	TBlobField *cdsCountriesNATIONALFLAG;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TStylesSimpleDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoMainDM *StylesSimpleDemoMainDM;
//---------------------------------------------------------------------------
#endif
