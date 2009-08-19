let $auction := doc("jungleshred:tmp#XMark")
for $b in $auction/site/open_auctions/open_auction
return <increase> { $b/bidder[1]/increase/text() } </increase>

