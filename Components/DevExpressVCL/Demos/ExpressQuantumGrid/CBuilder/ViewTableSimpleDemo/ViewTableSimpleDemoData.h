//---------------------------------------------------------------------------

#ifndef ViewTableSimpleDemoDataH
#define ViewTableSimpleDemoDataH
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
class TViewTableSimpleDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsGENRES;
  TDataSource *dsFilms;
	TClientDataSet *cdsGenres;
	TClientDataSet *cdsFilms;
	TClientDataSet *cdsFilmsGenres;
	TAutoIncField *cdsGenresID;
	TStringField *cdsGenresNAME;
	TAutoIncField *cdsFilmsGenresID;
	TIntegerField *cdsFilmsGenresFILMID;
	TIntegerField *cdsFilmsGenresGENREID;
	TBlobField *cdsFilmsGenresPHOTO;
	TBlobField *cdsFilmsGenresICON;
	void __fastcall cdsGenresAfterScroll(TDataSet *DataSet);
	void __fastcall cdsFilmsBeforePost(TDataSet *DataSet);
	void __fastcall cdsFilmsAfterPost(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
  bool FUpdating;
	void SetFilter();
public:		// User declarations
  __fastcall TViewTableSimpleDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewTableSimpleDemoMainDM *ViewTableSimpleDemoMainDM;
//---------------------------------------------------------------------------
#endif
 