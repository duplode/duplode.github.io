#!/bin/bash
rsync -av --delete --exclude '.git' _site/ ../master/
