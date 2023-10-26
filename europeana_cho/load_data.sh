#!/bin/sh

#set -x

$TDBLOADER --graph=http://intavia.eu/europeana/ /vol-data/europeana-export-for-intavia-persons-fixed.ttl

$TEXTINDEXER
$TDBSTATS --graph urn:x-arq:UnionGraph > /tmp/stats.opt \
 && mv /tmp/stats.opt /fuseki-base/databases/tdb/
