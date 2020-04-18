#lang racket

(provide (all-defined-out))

(require plisqin
         plisqin-examples/adventure-works)

(define-schema adventure-works-schema
  (table Department
         #:column
         [DepartmentID #:type Number #:null no]
         [GroupName #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table Employee
         #:column
         [BirthDate #:type Datetime #:null no]
         [BusinessEntityID #:type Number #:null no]
         [CurrentFlag #:null no]
         [Gender #:type String #:null no]
         [HireDate #:type Datetime #:null no]
         [JobTitle #:type String #:null no]
         [LoginID #:type String #:null no]
         [MaritalStatus #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [NationalIDNumber #:type String #:null no]
         [OrganizationLevel #:type Number #:null yes]
         [OrganizationNode #:null yes]
         [rowguid #:null no]
         [SalariedFlag #:null no]
         [SickLeaveHours #:type Number #:null no]
         [VacationHours #:type Number #:null no])
  (table EmployeeDepartmentHistory
         #:column
         [BusinessEntityID #:type Number #:null no]
         [DepartmentID #:type Number #:null no]
         [EndDate #:type Datetime #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [ShiftID #:type Number #:null no]
         [StartDate #:type Datetime #:null no])
  (table EmployeePayHistory
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PayFrequency #:type Number #:null no]
         [Rate #:type Number #:null no]
         [RateChangeDate #:type Datetime #:null no])
  (table JobCandidate
         #:column
         [BusinessEntityID #:type Number #:null yes]
         [JobCandidateID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Resume #:null yes])
  (table Shift
         #:column
         [EndTime #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ShiftID #:type Number #:null no]
         [StartTime #:null no])
  (table Address
         #:column
         [AddressID #:type Number #:null no]
         [AddressLine1 #:type String #:null no]
         [AddressLine2 #:type String #:null yes]
         [City #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PostalCode #:type String #:null no]
         [rowguid #:null no]
         [SpatialLocation #:null yes]
         [StateProvinceID #:type Number #:null no])
  (table AddressType
         #:column
         [AddressTypeID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no])
  (table BusinessEntity
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no])
  (table BusinessEntityAddress
         #:column
         [AddressID #:type Number #:null no]
         [AddressTypeID #:type Number #:null no]
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no])
  (table BusinessEntityContact
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ContactTypeID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PersonID #:type Number #:null no]
         [rowguid #:null no])
  (table ContactType
         #:column
         [ContactTypeID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table CountryRegion
         #:column
         [CountryRegionCode #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table EmailAddress
         #:column
         [BusinessEntityID #:type Number #:null no]
         [EmailAddress #:type String #:null yes]
         [EmailAddressID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no])
  (table Password
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PasswordHash #:type String #:null no]
         [PasswordSalt #:type String #:null no]
         [rowguid #:null no])
  (table Person
         #:column
         [AdditionalContactInfo #:null yes]
         [BusinessEntityID #:type Number #:null no]
         [Demographics #:null yes]
         [EmailPromotion #:type Number #:null no]
         [FirstName #:type String #:null no]
         [LastName #:type String #:null no]
         [MiddleName #:type String #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [NameStyle #:null no]
         [PersonType #:type String #:null no]
         [rowguid #:null no]
         [Suffix #:type String #:null yes]
         [Title #:type String #:null yes])
  (table PersonPhone
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PhoneNumber #:type String #:null no]
         [PhoneNumberTypeID #:type Number #:null no])
  (table PhoneNumberType
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [PhoneNumberTypeID #:type Number #:null no])
  (table StateProvince
         #:column
         [CountryRegionCode #:type String #:null no]
         [IsOnlyStateProvinceFlag #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no]
         [StateProvinceCode #:type String #:null no]
         [StateProvinceID #:type Number #:null no]
         [TerritoryID #:type Number #:null no])
  (table BillOfMaterials
         #:column
         [BillOfMaterialsID #:type Number #:null no]
         [BOMLevel #:type Number #:null no]
         [ComponentID #:type Number #:null no]
         [EndDate #:type Datetime #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [PerAssemblyQty #:type Number #:null no]
         [ProductAssemblyID #:type Number #:null yes]
         [StartDate #:type Datetime #:null no]
         [UnitMeasureCode #:type String #:null no])
  (table Culture
         #:column
         [CultureID #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table Document
         #:column
         [ChangeNumber #:type Number #:null no]
         [Document #:null yes]
         [DocumentLevel #:type Number #:null yes]
         [DocumentNode #:null no]
         [DocumentSummary #:type String #:null yes]
         [FileExtension #:type String #:null no]
         [FileName #:type String #:null no]
         [FolderFlag #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Owner #:type Number #:null no]
         [Revision #:type String #:null no]
         [rowguid #:null no]
         [Status #:type Number #:null no]
         [Title #:type String #:null no])
  (table Illustration
         #:column
         [Diagram #:null yes]
         [IllustrationID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no])
  (table Location
         #:column
         [Availability #:type Number #:null no]
         [CostRate #:type Number #:null no]
         [LocationID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table Product
         #:column
         [Class #:type String #:null yes]
         [Color #:type String #:null yes]
         [DaysToManufacture #:type Number #:null no]
         [DiscontinuedDate #:type Datetime #:null yes]
         [FinishedGoodsFlag #:null no]
         [ListPrice #:type Number #:null no]
         [MakeFlag #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ProductID #:type Number #:null no]
         [ProductLine #:type String #:null yes]
         [ProductModelID #:type Number #:null yes]
         [ProductNumber #:type String #:null no]
         [ProductSubcategoryID #:type Number #:null yes]
         [ReorderPoint #:type Number #:null no]
         [rowguid #:null no]
         [SafetyStockLevel #:type Number #:null no]
         [SellEndDate #:type Datetime #:null yes]
         [SellStartDate #:type Datetime #:null no]
         [Size #:type String #:null yes]
         [SizeUnitMeasureCode #:type String #:null yes]
         [StandardCost #:type Number #:null no]
         [Style #:type String #:null yes]
         [Weight #:type Number #:null yes]
         [WeightUnitMeasureCode #:type String #:null yes]
         #:has-one
         [ProductSubcategory
          (join subcat ProductSubcategory
                (join-type 'left)
                (join-on (.= (ProductSubcategoryID subcat)
                             (?? (ProductSubcategoryID this) /void))))]
         #:property
         [ProductName
          (Name this)]
         [SubcategoryName
          (SubcategoryName (ProductSubcategory this))]
         [CategoryName
          (CategoryName (ProductSubcategory this))])
  (table ProductCategory
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ProductCategoryID #:type Number #:null no]
         [rowguid #:null no]
         #:property
         [CategoryName
          (Name this)])
  (table ProductCostHistory
         #:column
         [EndDate #:type Datetime #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [StandardCost #:type Number #:null no]
         [StartDate #:type Datetime #:null no])
  (table ProductDescription
         #:column
         [Description #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductDescriptionID #:type Number #:null no]
         [rowguid #:null no])
  (table ProductDocument
         #:column
         [DocumentNode #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no])
  (table ProductInventory
         #:column
         [Bin #:type Number #:null no]
         [LocationID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [Quantity #:type Number #:null no]
         [rowguid #:null no]
         [Shelf #:type String #:null no])
  (table ProductListPriceHistory
         #:column
         [EndDate #:type Datetime #:null yes]
         [ListPrice #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [StartDate #:type Datetime #:null no])
  (table ProductModel
         #:column
         [CatalogDescription #:null yes]
         [Instructions #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ProductModelID #:type Number #:null no]
         [rowguid #:null no])
  (table ProductModelIllustration
         #:column
         [IllustrationID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductModelID #:type Number #:null no])
  (table ProductModelProductDescriptionCulture
         #:column
         [CultureID #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductDescriptionID #:type Number #:null no]
         [ProductModelID #:type Number #:null no])
  (table ProductPhoto
         #:column
         [LargePhoto #:null yes]
         [LargePhotoFileName #:type String #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [ProductPhotoID #:type Number #:null no]
         [ThumbNailPhoto #:null yes]
         [ThumbnailPhotoFileName #:type String #:null yes])
  (table ProductProductPhoto
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Primary #:null no]
         [ProductID #:type Number #:null no]
         [ProductPhotoID #:type Number #:null no])
  (table ProductReview
         #:column
         [Comments #:type String #:null yes]
         [EmailAddress #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [ProductReviewID #:type Number #:null no]
         [Rating #:type Number #:null no]
         [ReviewDate #:type Datetime #:null no]
         [ReviewerName #:type String #:null no])
  (table ProductSubcategory
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ProductCategoryID #:type Number #:null no]
         [ProductSubcategoryID #:type Number #:null no]
         [rowguid #:null no]
         #:has-one
         [ProductCategory
          (join cat ProductCategory
                (join-on (.= (ProductCategoryID cat)
                             (?? (ProductCategoryID this) /void))))]
         #:property
         [CategoryName
          (CategoryName (ProductCategory this))]
         [SubcategoryName
          (Name this)])
  (table ScrapReason
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ScrapReasonID #:type Number #:null no])
  (table TransactionHistory
         #:column
         [ActualCost #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [Quantity #:type Number #:null no]
         [ReferenceOrderID #:type Number #:null no]
         [ReferenceOrderLineID #:type Number #:null no]
         [TransactionDate #:type Datetime #:null no]
         [TransactionID #:type Number #:null no]
         [TransactionType #:type String #:null no])
  (table TransactionHistoryArchive
         #:column
         [ActualCost #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [Quantity #:type Number #:null no]
         [ReferenceOrderID #:type Number #:null no]
         [ReferenceOrderLineID #:type Number #:null no]
         [TransactionDate #:type Datetime #:null no]
         [TransactionID #:type Number #:null no]
         [TransactionType #:type String #:null no])
  (table UnitMeasure
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [UnitMeasureCode #:type String #:null no])
  (table WorkOrder
         #:column
         [DueDate #:type Datetime #:null no]
         [EndDate #:type Datetime #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [OrderQty #:type Number #:null no]
         [ProductID #:type Number #:null no]
         [ScrappedQty #:type Number #:null no]
         [ScrapReasonID #:type Number #:null yes]
         [StartDate #:type Datetime #:null no]
         [StockedQty #:type Number #:null no]
         [WorkOrderID #:type Number #:null no])
  (table WorkOrderRouting
         #:column
         [ActualCost #:type Number #:null yes]
         [ActualEndDate #:type Datetime #:null yes]
         [ActualResourceHrs #:type Number #:null yes]
         [ActualStartDate #:type Datetime #:null yes]
         [LocationID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OperationSequence #:type Number #:null no]
         [PlannedCost #:type Number #:null no]
         [ProductID #:type Number #:null no]
         [ScheduledEndDate #:type Datetime #:null no]
         [ScheduledStartDate #:type Datetime #:null no]
         [WorkOrderID #:type Number #:null no])
  (table ProductVendor
         #:column
         [AverageLeadTime #:type Number #:null no]
         [BusinessEntityID #:type Number #:null no]
         [LastReceiptCost #:type Number #:null yes]
         [LastReceiptDate #:type Datetime #:null yes]
         [MaxOrderQty #:type Number #:null no]
         [MinOrderQty #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OnOrderQty #:type Number #:null yes]
         [ProductID #:type Number #:null no]
         [StandardPrice #:type Number #:null no]
         [UnitMeasureCode #:type String #:null no])
  (table PurchaseOrderDetail
         #:column
         [DueDate #:type Datetime #:null no]
         [LineTotal #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OrderQty #:type Number #:null no]
         [ProductID #:type Number #:null no]
         [PurchaseOrderDetailID #:type Number #:null no]
         [PurchaseOrderID #:type Number #:null no]
         [ReceivedQty #:type Number #:null no]
         [RejectedQty #:type Number #:null no]
         [StockedQty #:type Number #:null no]
         [UnitPrice #:type Number #:null no])
  (table PurchaseOrderHeader
         #:column
         [EmployeeID #:type Number #:null no]
         [Freight #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OrderDate #:type Datetime #:null no]
         [PurchaseOrderID #:type Number #:null no]
         [RevisionNumber #:type Number #:null no]
         [ShipDate #:type Datetime #:null yes]
         [ShipMethodID #:type Number #:null no]
         [Status #:type Number #:null no]
         [SubTotal #:type Number #:null no]
         [TaxAmt #:type Number #:null no]
         [TotalDue #:type Number #:null no]
         [VendorID #:type Number #:null no])
  (table ShipMethod
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no]
         [ShipBase #:type Number #:null no]
         [ShipMethodID #:type Number #:null no]
         [ShipRate #:type Number #:null no])
  (table Vendor
         #:column
         [AccountNumber #:type String #:null no]
         [ActiveFlag #:null no]
         [BusinessEntityID #:type Number #:null no]
         [CreditRating #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [PreferredVendorStatus #:null no]
         [PurchasingWebServiceURL #:type String #:null yes])
  (table CountryRegionCurrency
         #:column
         [CountryRegionCode #:type String #:null no]
         [CurrencyCode #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no])
  (table CreditCard
         #:column
         [CardNumber #:type String #:null no]
         [CardType #:type String #:null no]
         [CreditCardID #:type Number #:null no]
         [ExpMonth #:type Number #:null no]
         [ExpYear #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no])
  (table Currency
         #:column
         [CurrencyCode #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no])
  (table CurrencyRate
         #:column
         [AverageRate #:type Number #:null no]
         [CurrencyRateDate #:type Datetime #:null no]
         [CurrencyRateID #:type Number #:null no]
         [EndOfDayRate #:type Number #:null no]
         [FromCurrencyCode #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ToCurrencyCode #:type String #:null no])
  (table Customer
         #:column
         [AccountNumber #:type String #:null no]
         [CustomerID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [PersonID #:type Number #:null yes]
         [rowguid #:null no]
         [StoreID #:type Number #:null yes]
         [TerritoryID #:type Number #:null yes])
  (table PersonCreditCard
         #:column
         [BusinessEntityID #:type Number #:null no]
         [CreditCardID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no])
  (table SalesOrderDetail
         #:column
         [CarrierTrackingNumber #:type String #:null yes]
         [LineTotal #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OrderQty #:type Number #:null no]
         [ProductID #:type Number #:null no]
         [rowguid #:null no]
         [SalesOrderDetailID #:type Number #:null no]
         [SalesOrderID #:type Number #:null no]
         [SpecialOfferID #:type Number #:null no]
         [UnitPrice #:type Number #:null no]
         [UnitPriceDiscount #:type Number #:null no])
  (table SalesOrderHeader
         #:column
         [AccountNumber #:type String #:null yes]
         [BillToAddressID #:type Number #:null no]
         [Comment #:type String #:null yes]
         [CreditCardApprovalCode #:type String #:null yes]
         [CreditCardID #:type Number #:null yes]
         [CurrencyRateID #:type Number #:null yes]
         [CustomerID #:type Number #:null no]
         [DueDate #:type Datetime #:null no]
         [Freight #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [OnlineOrderFlag #:null no]
         [OrderDate #:type Datetime #:null no]
         [PurchaseOrderNumber #:type String #:null yes]
         [RevisionNumber #:type Number #:null no]
         [rowguid #:null no]
         [SalesOrderID #:type Number #:null no]
         [SalesOrderNumber #:type String #:null no]
         [SalesPersonID #:type Number #:null yes]
         [ShipDate #:type Datetime #:null yes]
         [ShipMethodID #:type Number #:null no]
         [ShipToAddressID #:type Number #:null no]
         [Status #:type Number #:null no]
         [SubTotal #:type Number #:null no]
         [TaxAmt #:type Number #:null no]
         [TerritoryID #:type Number #:null yes]
         [TotalDue #:type Number #:null no])
  (table SalesOrderHeaderSalesReason
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [SalesOrderID #:type Number #:null no]
         [SalesReasonID #:type Number #:null no])
  (table SalesPerson
         #:column
         [Bonus #:type Number #:null no]
         [BusinessEntityID #:type Number #:null no]
         [CommissionPct #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no]
         [SalesLastYear #:type Number #:null no]
         [SalesQuota #:type Number #:null yes]
         [SalesYTD #:type Number #:null no]
         [TerritoryID #:type Number #:null yes])
  (table SalesPersonQuotaHistory
         #:column
         [BusinessEntityID #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [QuotaDate #:type Datetime #:null no]
         [rowguid #:null no]
         [SalesQuota #:type Number #:null no])
  (table SalesReason
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [ReasonType #:type String #:null no]
         [SalesReasonID #:type Number #:null no])
  (table SalesTaxRate
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no]
         [SalesTaxRateID #:type Number #:null no]
         [StateProvinceID #:type Number #:null no]
         [TaxRate #:type Number #:null no]
         [TaxType #:type Number #:null no])
  (table SalesTerritory
         #:column
         [CostLastYear #:type Number #:null no]
         [CostYTD #:type Number #:null no]
         [CountryRegionCode #:type String #:null no]
         [Group #:type String #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no]
         [SalesLastYear #:type Number #:null no]
         [SalesYTD #:type Number #:null no]
         [TerritoryID #:type Number #:null no])
  (table SalesTerritoryHistory
         #:column
         [BusinessEntityID #:type Number #:null no]
         [EndDate #:type Datetime #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no]
         [StartDate #:type Datetime #:null no]
         [TerritoryID #:type Number #:null no])
  (table ShoppingCartItem
         #:column
         [DateCreated #:type Datetime #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [Quantity #:type Number #:null no]
         [ShoppingCartID #:type String #:null no]
         [ShoppingCartItemID #:type Number #:null no])
  (table SpecialOffer
         #:column
         [Category #:type String #:null no]
         [Description #:type String #:null no]
         [DiscountPct #:type Number #:null no]
         [EndDate #:type Datetime #:null no]
         [MaxQty #:type Number #:null yes]
         [MinQty #:type Number #:null no]
         [ModifiedDate #:type Datetime #:null no]
         [rowguid #:null no]
         [SpecialOfferID #:type Number #:null no]
         [StartDate #:type Datetime #:null no]
         [Type #:type String #:null no])
  (table SpecialOfferProduct
         #:column
         [ModifiedDate #:type Datetime #:null no]
         [ProductID #:type Number #:null no]
         [rowguid #:null no]
         [SpecialOfferID #:type Number #:null no])
  (table Store
         #:column
         [BusinessEntityID #:type Number #:null no]
         [Demographics #:null yes]
         [ModifiedDate #:type Datetime #:null no]
         [Name #:type String #:null no]
         [rowguid #:null no]
         [SalesPersonID #:type Number #:null yes]))
