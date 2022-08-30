//---------------------------------------------------------------------------

#ifndef EditorsInPlaceValidationDemoDataH
#define EditorsInPlaceValidationDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxVGrid.hpp"
#include <ImgList.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TEditorsInPlaceValidationDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TcxStyleRepository *StyleRepository;
  TcxStyle *styCaption;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
	TdxMemData *dxMemData1;
	TStringField *dxMemData1FirstName;
	TStringField *dxMemData1LastName;
	TStringField *dxMemData1Address;
	TStringField *dxMemData1PhoneNumber;
	TStringField *dxMemData1Email;
	TDataSource *DataSource;
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceValidationDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceValidationDemoDataDM *EditorsInPlaceValidationDemoDataDM;
//---------------------------------------------------------------------------
#endif
