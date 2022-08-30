//---------------------------------------------------------------------------

#ifndef FrameAnimationDemoDMH
#define FrameAnimationDemoDMH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DB.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TDM : public TDataModule
{
__published:	// IDE-managed Components
	TDataSource *dsHomePhotos;
	TClientDataSet *clHomePhotos;
	TDataSource *dsHomesAndAgents;
	TClientDataSet *clHomesAndAgents;
	TDataSource *dsHomesAndHomes;
	TClientDataSet *clHomesAndHomes;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDM *DM;
//---------------------------------------------------------------------------
#endif
