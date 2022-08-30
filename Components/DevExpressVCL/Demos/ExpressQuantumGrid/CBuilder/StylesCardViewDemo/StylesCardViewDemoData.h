//---------------------------------------------------------------------------

#ifndef StylesCardViewDemoDataH
#define StylesCardViewDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxDBEditRepository.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGridCardView.hpp"
#include <ImgList.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TStylesCardViewDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsPersons;
  TcxStyleRepository *StyleRepository;
  TcxStyle *stBlueDark;
  TcxStyle *stGold;
  TcxStyle *stBlueLight;
  TcxStyle *stBlueBright;
  TcxStyle *stYellowLight;
  TcxStyle *stGreyLight;
  TcxStyle *stBlueSky;
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
  TcxStyle *cxStyle14;
  TcxStyle *cxStyle15;
  TcxStyle *cxStyle16;
  TcxStyle *cxStyle17;
  TcxStyle *cxStyle18;
  TcxStyle *cxStyle19;
  TcxStyle *cxStyle20;
  TcxStyle *cxStyle21;
  TcxStyle *cxStyle22;
  TcxStyle *cxStyle23;
  TcxStyle *cxStyle24;
  TcxStyle *cxStyle25;
  TcxStyle *cxStyle26;
  TcxStyle *cxStyle27;
  TcxStyle *cxStyle28;
  TcxStyle *cxStyle29;
  TcxStyle *cxStyle30;
  TcxStyle *cxStyle31;
  TcxStyle *cxStyle32;
  TcxStyle *cxStyle33;
  TcxStyle *cxStyle34;
  TcxStyle *cxStyle35;
  TcxStyle *cxStyle36;
  TcxStyle *cxStyle37;
  TcxStyle *cxStyle38;
  TcxStyle *cxStyle39;
  TcxStyle *cxStyle40;
  TcxStyle *cxStyle41;
  TcxStyle *cxStyle42;
  TcxStyle *cxStyle43;
  TcxStyle *cxStyle44;
  TcxStyle *cxStyle45;
  TcxStyle *cxStyle46;
  TcxStyle *cxStyle47;
  TcxGridCardViewStyleSheet *cvssDevExpress;
  TcxGridCardViewStyleSheet *cvssSlate;
  TcxGridCardViewStyleSheet *cvssHighContrast;
  TcxGridCardViewStyleSheet *cvssUserDefined;
  TDataSource *dsCountries;
  TcxEditRepository *EditRepository;
  TcxEditRepositoryImageComboBoxItem *edrepGender;
  TcxEditRepositoryLookupComboBoxItem *edrepCountry;
  TImageList *ilPics;
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
  TStringField *cdsPersonsFullName;
  TClientDataSet *cdsCountries;
  TAutoIncField *cdsCountriesID;
  TStringField *cdsCountriesNAME;
  TStringField *cdsCountriesACRONYM;
  TBlobField *cdsCountriesNATIONALFLAG;
  void __fastcall DataModuleCreate(TObject *Sender);
  void __fastcall cdsPersonsCalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
  __fastcall TStylesCardViewDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesCardViewDemoMainDM *StylesCardViewDemoMainDM;
//---------------------------------------------------------------------------
#endif
